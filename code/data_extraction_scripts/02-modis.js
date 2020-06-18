var imageCollection = ee.ImageCollection("MODIS/051/MCD12Q1");
// Load 2000 MODIS land cover and select the IGBP classification.
var cover = ee.Image('MODIS/051/MCD12Q1/2013_01_01')
  .select('Land_Cover_Type_1')

Map.addLayer(cover)

// For all cells BioTIME cells
var cells =
    ee.FeatureCollection('ft:1SCfjD41FViCXJEEciu7NVg_qjPOiHNTuWDpjquyl');

var stats = ee.Image.pixelArea().addBands(cover).reduceRegions({
  reducer: ee.Reducer.sum().group(1),
  collection: cells,
  scale: 500,
});

function toProperties(f) {
  // Output of the grouped reducer.
  var listOfObjects = ee.List(f.get('groups'));

  // Turn the list of objects into a dictionary keyed by group name.
  var propertiesDict = ee.Dictionary(
    listOfObjects.map(function(obj) {
      obj = ee.Dictionary(obj);
      var group = ee.Number(obj.get('group')).format();
      return [group, obj.get('sum')];
    }).flatten())
  return ee.Feature(f.geometry()).set(propertiesDict).copyProperties(f, ['rarefyID'])
}
print(stats)
var statsTable = stats.map(toProperties)

Export.table.toDrive(statsTable);
