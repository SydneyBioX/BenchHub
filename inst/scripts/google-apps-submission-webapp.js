const SPREADSHEET_ID = '1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg';

const ID_PREFIX = {
  Study: 'ST',
  Dataset: 'D',
  DatasetTask: 'DT',
  DatasetEvidence: 'E',
  Metric: 'M',
  StudyDataset: 'SD',
  DatasetTaskMetric: 'DTM'
};

function doGet(e) {
  return ContentService
    .createTextOutput(JSON.stringify({
      ok: true,
      message: 'Web app is running. Send a POST request to submit data.'
    }))
    .setMimeType(ContentService.MimeType.JSON);
}

function doPost(e) {
  const lock = LockService.getScriptLock();
  lock.waitLock(30000);

  try {
    const ss = SpreadsheetApp.openById(SPREADSHEET_ID);
    const sheet = ss.getSheetByName('Submission_Master');
    if (!sheet) {
      throw new Error('Sheet not found: Submission_Master');
    }

    const body = JSON.parse(e.postData.contents || '{}');

    const submissionID = generateSubmissionID_();
    const submittedTime = new Date();
    const submittedBy = body.submittedBy || '';
    const submittedType = body.submittedType || '';
    const status = 'pending';
    const reviewedBy = '';
    const reviewedTime = '';
    const syncStatus = 'not_synced';
    const notes = '';
    const payloadJson = JSON.stringify(body.payload || {});

    sheet.appendRow([
      submissionID,
      submittedTime,
      submittedBy,
      submittedType,
      status,
      reviewedBy,
      reviewedTime,
      syncStatus,
      notes,
      payloadJson
    ]);

    return ContentService
      .createTextOutput(JSON.stringify({
        ok: true,
        submissionID: submissionID,
        message: 'Submission received'
      }))
      .setMimeType(ContentService.MimeType.JSON);

  } catch (err) {
    return ContentService
      .createTextOutput(JSON.stringify({
        ok: false,
        error: String(err)
      }))
      .setMimeType(ContentService.MimeType.JSON);
  } finally {
    lock.releaseLock();
  }
}

function syncApprovedSubmissions() {
  const lock = LockService.getScriptLock();
  lock.waitLock(30000);

  try {
    const ss = SpreadsheetApp.openById(SPREADSHEET_ID);
    const master = ss.getSheetByName('Submission_Master');
    if (!master) {
      throw new Error('Sheet not found: Submission_Master');
    }

    const values = master.getDataRange().getValues();
    if (values.length === 0) {
      return;
    }

    const header = values[0];
    const col = indexMap_(header);

    for (let i = 1; i < values.length; i++) {
      const row = values[i];
      const status = row[col.status];
      const syncStatus = row[col.syncStatus];

      if (status !== 'approved' || syncStatus === 'synced') {
        continue;
      }

      try {
        const container = JSON.parse(row[col.payload_json] || '{}');
        const payload = container.payload ? container.payload : container;
        const submissionLinks = container.submission_links || {};

        validatePayload_(payload, submissionLinks);

        const resolved = resolveSubmissionPayload_(ss, payload, submissionLinks);

        if (resolved.Study.length > 0) {
          appendArrayToSheet_(ss, 'Study', resolved.Study);
        }

        appendArrayToSheet_(ss, 'Dataset', resolved.Dataset);
        appendArrayToSheet_(ss, 'DatasetTask', resolved.DatasetTask);
        appendArrayToSheet_(ss, 'DatasetEvidence', resolved.DatasetEvidence);
        appendArrayToSheet_(ss, 'Metric', resolved.Metric);

        if (resolved.StudyDataset.length > 0) {
          appendArrayToSheet_(ss, 'StudyDataset', resolved.StudyDataset);
        }

        appendArrayToSheet_(ss, 'DatasetTaskMetric', resolved.DatasetTaskMetric);

        master.getRange(i + 1, col.syncStatus + 1).setValue('synced');
        master.getRange(i + 1, col.notes + 1).setValue('');
        Logger.log('Row ' + (i + 1) + ' synced successfully');

      } catch (err) {
        master.getRange(i + 1, col.syncStatus + 1).setValue('error');
        master.getRange(i + 1, col.notes + 1).setValue(String(err));
        Logger.log('Failed to sync row ' + (i + 1) + ': ' + err);
      }
    }
  } finally {
    lock.releaseLock();
  }
}

function resolveSubmissionPayload_(ss, payload, submissionLinks) {
  const datasetRows = payload.Dataset || [];
  const taskRows = payload.DatasetTask || [];
  const evidenceRows = payload.DatasetEvidence || [];
  const metricRows = payload.Metric || payload.Metrics || [];
  const studyRows = toArray_(payload.Study);
  const studyDatasetRows = payload.StudyDataset || [];
  const taskMetricRows = payload.DatasetTaskMetric || [];

  const nextStudyId = makeIdAllocator_(ss, 'Study', ID_PREFIX.Study);
  const nextDatasetId = makeIdAllocator_(ss, 'Dataset', ID_PREFIX.Dataset);
  const nextTaskId = makeIdAllocator_(ss, 'DatasetTask', ID_PREFIX.DatasetTask);
  const nextEvidenceId = makeIdAllocator_(ss, 'DatasetEvidence', ID_PREFIX.DatasetEvidence);
  const nextMetricId = makeIdAllocator_(ss, 'Metric', ID_PREFIX.Metric);
  const nextStudyDatasetId = makeIdAllocator_(ss, 'StudyDataset', ID_PREFIX.StudyDataset);
  const nextTaskMetricId = makeIdAllocator_(ss, 'DatasetTaskMetric', ID_PREFIX.DatasetTaskMetric);

  const existingMetricKeyIndex = buildExistingMetricKeyIndex_(ss);

  const resolved = {
    Study: [],
    Dataset: [],
    DatasetTask: [],
    DatasetEvidence: [],
    Metric: [],
    StudyDataset: [],
    DatasetTaskMetric: []
  };

  const taskMap = {};
  const metricMap = {};
  const metricKeyToId = {};

  resolved.Study = studyRows.map(function(row) {
    const studyID = row.studyID || nextStudyId();
    return Object.assign({}, row, { studyID: studyID });
  });

  resolved.Dataset = datasetRows.map(function(row) {
    const datasetID = row.datasetID || nextDatasetId();
    return Object.assign({}, row, { datasetID: datasetID });
  });

  const resolvedDatasetID = resolved.Dataset.length > 0 ? resolved.Dataset[0].datasetID : '';

  resolved.DatasetTask = taskRows.map(function(row, idx) {
    const tempLink = (submissionLinks.task || [])[idx];
    const datasetTaskID = row.datasetTaskID || nextTaskId();

    if (tempLink && tempLink.task_temp_id) {
      taskMap[tempLink.task_temp_id] = datasetTaskID;
    }

    return Object.assign({}, row, {
      datasetTaskID: datasetTaskID,
      datasetID: resolvedDatasetID
    });
  });

  resolved.DatasetEvidence = evidenceRows.map(function(row) {
    const evidenceID = row.evidenceID || nextEvidenceId();

    let datasetTaskID = row.datasetTaskID;
    if (!datasetTaskID) {
      const evidenceLink = (submissionLinks.evidence || []).find(function(x) {
        return x.evidenceName === row.supportingEvidence;
      });
      if (evidenceLink && evidenceLink.task_temp_id) {
        datasetTaskID = taskMap[evidenceLink.task_temp_id];
      }
    }

    if (!datasetTaskID) {
      throw new Error('Could not resolve datasetTaskID for evidence: ' + row.supportingEvidence);
    }

    return Object.assign({}, row, {
      evidenceID: evidenceID,
      datasetTaskID: datasetTaskID
    });
  });

  metricRows.forEach(function(row, idx) {
    const tempLink = (submissionLinks.metric || [])[idx] || {};
    const metricKey = buildMetricKey_(row);
    let metricID = metricKeyToId[metricKey] || existingMetricKeyIndex[metricKey];

    if (!metricID) {
      metricID = row.metricID || nextMetricId();
      metricKeyToId[metricKey] = metricID;
      resolved.Metric.push(Object.assign({}, row, {
        metricID: metricID
      }));
    }

    if (tempLink.metric_temp_id) {
      metricMap[tempLink.metric_temp_id] = metricID;
    }

    metricMap[metricKey] = metricID;

    if (row.metricName) {
      metricMap['metricName|' + String(row.metricName).trim()] = metricID;
    }

    if (tempLink.metricName) {
      metricMap['metricName|' + String(tempLink.metricName).trim()] = metricID;
    }
  });

  resolved.StudyDataset = studyDatasetRows.map(function(row) {
    const studyDatasetID = row.studyDatasetID || nextStudyDatasetId();
    const studyID = row.studyID || (resolved.Study[0] ? resolved.Study[0].studyID : '');
    const datasetID = row.datasetID || resolvedDatasetID;

    return Object.assign({}, row, {
      studyDatasetID: studyDatasetID,
      studyID: studyID,
      datasetID: datasetID
    });
  });

  resolved.DatasetTaskMetric = resolveTaskMetricRows_(
    taskMetricRows,
    submissionLinks,
    taskMap,
    metricMap,
    nextTaskMetricId
  );

  return resolved;
}

function resolveTaskMetricRows_(taskMetricRows, submissionLinks, taskMap, metricMap, nextTaskMetricId) {
  const taskMetricLinks = submissionLinks.task_metric || [];

  if (taskMetricLinks.length > 0) {
    return taskMetricLinks.map(function(link) {
      const datasetTaskID = taskMap[link.task_temp_id];
      const metricKey = buildMetricKey_(link);
      const metricID =
        metricMap[link.metric_temp_id] ||
        metricMap[metricKey] ||
        metricMap['metricName|' + String(link.metricName || '').trim()];

      if (!datasetTaskID) {
        throw new Error('Could not resolve datasetTaskID for DatasetTaskMetric');
      }
      if (!metricID) {
        throw new Error('Could not resolve metricID for DatasetTaskMetric');
      }

      return {
        datasetTaskMetricID: nextTaskMetricId(),
        datasetTaskID: datasetTaskID,
        metricID: metricID
      };
    });
  }

  const hasUsableIds = taskMetricRows.every(function(row) {
    return row.datasetTaskID && row.metricID;
  });

  if (hasUsableIds) {
    return taskMetricRows.map(function(row) {
      return {
        datasetTaskMetricID: row.datasetTaskMetricID || nextTaskMetricId(),
        datasetTaskID: taskMap[row.datasetTaskID] || row.datasetTaskID,
        metricID: metricMap[row.metricID] || row.metricID
      };
    });
  }

  if (taskMetricRows.length > 0) {
    throw new Error(
      'DatasetTaskMetric cannot be resolved. Please provide submission_links.task_metric or usable task/metric IDs.'
    );
  }

  return [];
}

function validatePayload_(payload, submissionLinks) {
  if (!payload) {
    throw new Error('payload is empty');
  }

  if (!payload.Dataset || payload.Dataset.length === 0) {
    throw new Error('Dataset is required');
  }

  if (!payload.DatasetTask || payload.DatasetTask.length === 0) {
    throw new Error('DatasetTask is required');
  }

  if (!payload.DatasetEvidence || payload.DatasetEvidence.length === 0) {
    throw new Error('DatasetEvidence is required');
  }

  const metricRows = payload.Metric || payload.Metrics || [];
  if (metricRows.length === 0) {
    throw new Error('Metric is required');
  }

  const hasTaskMetricRows = !!(payload.DatasetTaskMetric && payload.DatasetTaskMetric.length > 0);
  const hasTaskMetricLinks = !!(submissionLinks && submissionLinks.task_metric && submissionLinks.task_metric.length > 0);
  if (!hasTaskMetricRows && !hasTaskMetricLinks) {
    throw new Error('DatasetTaskMetric is required');
  }

  if (!submissionLinks || !submissionLinks.task || !submissionLinks.evidence || !submissionLinks.metric) {
    throw new Error('submission_links.task, submission_links.evidence, and submission_links.metric are required');
  }

  metricRows.forEach(function(row, idx) {
    try {
      buildMetricKey_(row);
    } catch (err) {
      throw new Error('Metric row ' + (idx + 1) + ': ' + err.message);
    }
  });
}

function appendObjectToSheet_(ss, sheetName, obj) {
  if (!obj) return;
  appendArrayToSheet_(ss, sheetName, [obj]);
}

function appendArrayToSheet_(ss, sheetName, arr) {
  if (!arr || arr.length === 0) {
    Logger.log('Skip sheet ' + sheetName + ': empty array');
    return;
  }

  const sheet = ss.getSheetByName(sheetName);
  if (!sheet) {
    throw new Error('Sheet not found: ' + sheetName);
  }

  const header = sheet.getRange(1, 1, 1, sheet.getLastColumn()).getValues()[0];
  const rows = arr.map(function(item) {
    return header.map(function(colName) {
      return item[colName] !== undefined && item[colName] !== null ? item[colName] : '';
    });
  });

  sheet.getRange(sheet.getLastRow() + 1, 1, rows.length, header.length).setValues(rows);
}

function makeIdAllocator_(ss, sheetName, prefix) {
  const sheet = ss.getSheetByName(sheetName);
  if (!sheet) {
    throw new Error('Sheet not found: ' + sheetName);
  }

  const values = sheet.getDataRange().getValues();
  let maxNum = 0;

  for (let i = 1; i < values.length; i++) {
    const id = String(values[i][0] || '');
    const match = id.match(/(\d+)$/);
    if (match) {
      maxNum = Math.max(maxNum, parseInt(match[1], 10));
    }
  }

  let current = maxNum;
  return function() {
    current += 1;
    return prefix + String(current).padStart(3, '0');
  };
}

function buildExistingMetricKeyIndex_(ss) {
  const sheet = ss.getSheetByName('Metric');
  if (!sheet) {
    throw new Error('Sheet not found: Metric');
  }

  const values = sheet.getDataRange().getValues();
  if (values.length <= 1) {
    return {};
  }

  const header = values[0];
  const rows = values.slice(1);
  const index = {};

  rows.forEach(function(rowValues) {
    const row = rowToObject_(header, rowValues);
    const metricID = row.metricID || row[header[0]];

    if (!metricID) {
      return;
    }

    try {
      const key = buildMetricKey_(row);
      index[key] = metricID;
    } catch (err) {
      Logger.log('Skipping metric key build for existing row ' + metricID + ': ' + err);
    }
  });

  return index;
}

function buildMetricKey_(row) {
  const sourceType = inferMetricSourceType_(row);
  const metricName = normalizeString_(row.metricName);
  const metricKey = normalizeString_(firstNonEmpty_(row.metricKey, row.internalMetricKey, row.gistMetricKey, row.wrapper_r));
  const gistUrl = normalizeString_(firstNonEmpty_(row.gist_url, row.gistUrl));
  const gistId = extractGistId_(gistUrl);

  if (sourceType === 'internal') {
    const key = metricKey || metricName;
    if (!key) {
      throw new Error('internal metrics require metricKey or metricName');
    }
    return 'internal|' + key;
  }

  if (sourceType === 'gist') {
    const key = metricKey || (gistId && metricName ? gistId + '|' + metricName : '') || gistId || metricName;
    if (!key) {
      throw new Error('gist metrics require metricKey, gist_url, or metricName');
    }
    return 'gist|' + key;
  }

  const genericKey = (sourceType ? sourceType + '|' : 'unknown|') + metricName;
  if (!metricName) {
    throw new Error('metricName is required');
  }
  return genericKey;
}

function inferMetricSourceType_(row) {
  const explicit = normalizeMetricSourceType_(row.metricSourceType);
  if (explicit) {
    return explicit;
  }

  if (firstNonEmpty_(row.gist_url, row.gistUrl, row.gistMetricKey)) {
    return 'gist';
  }

  if (firstNonEmpty_(row.metricKey, row.internalMetricKey, row.wrapper_r)) {
    return 'internal';
  }

  return '';
}

function normalizeMetricSourceType_(value) {
  return normalizeString_(value).toLowerCase();
}

function normalizeString_(value) {
  if (value === null || value === undefined) {
    return '';
  }
  return String(value).trim();
}

function firstNonEmpty_() {
  for (let i = 0; i < arguments.length; i++) {
    const value = normalizeString_(arguments[i]);
    if (value) {
      return value;
    }
  }
  return '';
}

function rowToObject_(header, values) {
  const obj = {};
  header.forEach(function(name, idx) {
    obj[name] = values[idx];
  });
  return obj;
}

function extractGistId_(gistUrl) {
  const normalized = normalizeString_(gistUrl);
  if (!normalized) {
    return '';
  }

  const parts = normalized.split('/').filter(function(part) {
    return !!part;
  });

  return parts.length > 0 ? parts[parts.length - 1] : '';
}

function toArray_(x) {
  if (!x) return [];
  return Array.isArray(x) ? x : [x];
}

function indexMap_(header) {
  const map = {};
  header.forEach(function(name, idx) {
    map[name] = idx;
  });
  return map;
}

function generateSubmissionID_() {
  return 'SUB_' + Utilities.formatDate(
    new Date(),
    Session.getScriptTimeZone(),
    'yyyyMMdd_HHmmss_SSS'
  );
}
