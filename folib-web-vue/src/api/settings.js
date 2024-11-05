import { axios } from '@/utils/request'


export function getServerSettings () {
  return axios({
    url: '/api/configuration/folib/serverSettings',
    method: 'get'
  })
}
export function postServerSettings (data) {
  return axios({
    url: '/api/configuration/folib/serverSettings',
    method: 'post',
    data:data
  })
}


export function getServerName () {
  return axios({
    url: '/api/configuration/folib/getServerName',
    method: 'get',
  })
}


export function getLdap () {
  return axios({
    url: '/api/configuration/ldap',
    method: 'get'
  })
}
export function putLdap (data) {
  return axios({
    url: '/api/configuration/ldap',
    method: 'put',
    data:data
  })
}
export function getCrontasks () {
  return axios({
    url: '/api/configuration/crontasks/',
    method: 'get'
  })
}
export function delUser (username) {
  return axios({
    url: '/api/users/'+username,
    method: 'delete'
  })
}

export function getMachineCode () {
  return axios({
    url: '/api/ping/machineCode',
    method: 'get'
  })
}

export function postActivate (key,isPoc) {
  return axios({
    url: '/api/ping/activate?key='+key+'&isPoc='+isPoc,
    method: 'get'
  })
}

export function checkMachineCode () {
  return axios({
    url: '/api/ping/checkMachineCode',
    method: 'get'
  })
}

export function getMetadataConfiguration () {
  return axios({
    url: '/api/artifact/getMetadataConfiguration',
    method: 'get'
  })
}

export function globalSettingAddOrUpdateMetadata (data) {
  return axios({
    url: '/api/artifact/globalSettingAddOrUpdateMetadata',
    method: 'put',
    data: data
  })
}

export function globalSettingDeleteMetadata (data) {
  return axios({
    url: '/api/artifact/globalSettingDeleteMetadata',
    method: 'delete',
    data: data
  })
}

export function getArtifactDispatchConfig(){
  return axios({
    url: '/api/configuration/folib/dispatch',
    method: 'get'
  })
}

export function globalSettingArtifactDispatchConfig (data) {
  return axios({
    url: '/api/configuration/folib/dispatch',
    method: 'put',
    data: data
  })
}

export function globalSettingDelArtifactDispatchConfig(clusterEnName) {
  return axios({
    url: '/api/configuration/folib/dispatch/' + clusterEnName,
    method: 'delete'
  })
}

export function vulnerabilitiesDataUpdate(params) {
  return axios({
    url: '/api/folibScanner/update',
    method: 'get',
    params: params
  })
}

export function artifactScan(params) {
  return axios({
    url: '/api/folibScanner/scan',
    method: 'get',
    params: params
  })
}

export function getDataIndexDump(param) {
  return axios({
    url: '/api/cmdsevice/getIndexFile',
    method: 'get',
    params:param
  })
}


export function uploadJsonFile(data) {
  return axios({
    url: '/api/cmdsevice/uploadJsonFile',
    method: 'post',
    data:data
  })
}

export function getAllowAnonymous () {
  return axios({
    url: '/api/configuration/folib/allowAnonymous',
    method: 'get'
  })
}

export function getArtifactSyncRecordPage(dataFilter){
  return axios({
    url: '/api/artifactSyncRecord/page?'+objToUrlQuery(dataFilter),
    method: 'get'
  })
}

function objToUrlQuery(obj) {
  if (undefined !== obj && obj) {
    return Object.keys(obj).map(k => k+"="+obj[k]).join("&")
  }
  return ""
}

export function getArtifactSyncRecordCount(){
  return axios({
    url: '/api/artifactSyncRecord/getCount/30',
    method: 'get'
  })
}

export function getStatusTrends(){
  return axios({
    url: '/api/artifactSyncRecord/getStatusTrends/30',
    method: 'get'
  })
}

export function fileSizeStatisticsByWarehouse(days, limitNumber){
  return axios({
    url: '/api/artifactSyncRecord/fileSizeStatisticsByWarehouse/'+days+'/'+limitNumber,
    method: 'get',
  })
}

export function getCrontaskByClass(params) {
  return axios({
    url: '/api/configuration/crontasks/getByClass',
    method: 'get',
    params: params
  })
}

export function uploadLicenseFile(data){
  return axios({
    url: '/api/ping/offlineActivate',
    method: 'post',
    headers: {"Content-type": "multipart/form-data",},
    data: data
  })
}

export function restore (storageId,repositoryId,path) {
  return axios({
    url: '/api/browse/restore/'+storageId+'/'+repositoryId+'/'+path,
    method: 'post'
  })
}