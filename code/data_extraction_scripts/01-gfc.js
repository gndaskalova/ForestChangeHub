// Extracting overall forest gain and forest loss for 96km2 cells
// corresponding to the bioTIME cells in terrestrial regions > 1999
// Gergana Daskalova 10th Feb 2018

// Load BioTIME cell boundaries boundaries from a Fusion Table.
// Coordinates formates as per kml file
var cells =
    ee.FeatureCollection('ft:1SCfjD41FViCXJEEciu7NVg_qjPOiHNTuWDpjquyl');

// Get the loss and gain images.
var gfc = ee.Image('UMD/hansen/global_forest_change_2016_v1_4');
var lossImage = gfc.select(['loss']);
var gainImage = gfc.select(['gain']);
var areaImageLoss = lossImage.multiply(ee.Image.pixelArea());
var areaImageGain = gainImage.multiply(ee.Image.pixelArea());

// Sum the values of loss pixels.
var statsLoss = areaImageLoss.reduceRegions({
  reducer: ee.Reducer.sum(),
  collection: cells,
  scale: 30
});
Export.table.toDrive(statsLoss);

// Sum the values of gain pixels.
var statsGain = areaImageGain.reduceRegions({
  reducer: ee.Reducer.sum(),
  collection: cells,
  scale: 30
});
Export.table.toDrive(statsLoss);
Export.table.toDrive(statsGain);
