import { axios } from '@/utils/request'


export function getStorages () {
  return axios({
    url: '/api/configuration/folib/storages',
    method: 'get'
  })
}
export function getLibrary (libId) {
  return axios({
    url: '/api/configuration/folib/storages/'+libId,
    method: 'get'
  })
}

export function getRepositoryResponseEntity (storageId,repositoryId) {
  return axios({
    url: '/api/configuration/folib/storages/'+storageId+'/'+repositoryId,
    method: 'get'
  })
}

export function delRepositoryResponseEntity (storageId,repositoryId,force) {
  return axios({
    url: '/api/configuration/folib/storages/'+storageId+'/'+repositoryId+'?force='+force,
    method: 'delete'
  })
}

export function getLibraryByQuery (dataQuery) {
  return axios({
    url: '/api/formData/repositoryList',
    method: 'get',
    params: dataQuery
  })
}

export function getStoragesAndRepositories (dataQuery) {
  return axios({
    url: '/api/configuration/folib/storages/getStoragesAndRepositories',
    method: 'get',
    params: dataQuery
  })
}

export function createStorages (data) {
  return axios({
    url: '/api/configuration/folib/storages',
    method: 'put',
    data: data
  })
}

export function updateStorages (data) {
  return axios({
    url: '/api/configuration/folib/storages/'+data.id,
    method: 'put',
    data: data
  })
}
export function deleteStorages (data,force) {
  return axios({
    url: '/api/configuration/folib/storages/'+data.id+'?force='+force,
    method: 'delete'
  })
}

export function addOrUpdateRepository (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/'+storageId+'/'+repositoryId,
    method: 'put',
    data: obj
  })
}



export function getBaseUrl () {
  return axios({
    url: '/api/configuration/folib/baseUrl',
    method: 'get'
  })
}


export function browse (storageId,repositoryId,path) {
  return axios({
    url: '/api/browse/'+storageId+'/'+repositoryId+'/'+path,
    method: 'get'
  })
}
export function getArtifact (type,storageId,repositoryId,path){
  return axios({
    url: '/api/browse/getArtifact/'+storageId+'/'+repositoryId+'/'+path+'?type='+type,
    method: 'get'
  })
}

export function getDockerArtifact (storageId,repositoryId,path){
  return axios({
    url: '/api/browse/getDockerArtifact/'+storageId+'/'+repositoryId+'/'+path,
    method: 'get'
  })
}
export function viewArtifactFile (url) {
  return axios({
    url: url,
    method: 'get'
  })
}
//aql搜索接口
export function fql (query) {
  return axios({
    url: '/api/fql',
    method: 'get',
    params: query
  })
}
export function deleteArtifact (storageId,repositoryId,path){
  return axios({
    url: '/api/browse/'+storageId+'/'+repositoryId+'/'+path,
    method: 'delete'
  })
}

export function scannerRules (id) {
  return axios({
    url: '/api/scanRules/'+id,
    method: 'get'
  })
}

export function insertOrUpdateRules (data) {
  return axios({
    url: '/api/scanRules/insertOrUpdate',
    method: 'post',
    data: data
  })
}

export function getCount () {
  return axios({
    url: '/api/folibScanner/getCount',
    method: 'get'
  })
}

export function getScannerSumDifVoList () {
  return axios({
    url: '/api/folibScanner/getScannerSumDifVoList',
    method: 'get'
  })
}

export function vulnerabilityPage (query) {
  return axios({
    url: '/api/vulnerability/page',
    method: 'get',
    params: query
  })
}

export function vulnerabilityExportExcel (query) {
  return axios({
    url: '/api/artifact/exportExcel',
    method: 'get',
    params: query,
    responseType: 'blob'
  })
}

export function weekDayCount () {
  return axios({
    url: '/api/folibScanner/weekDayCount',
    method: 'get'
  })
}

export function mounthDayCount () {
  return axios({
    url: '/api/folibScanner/mounthDayCount',
    method: 'get'
  })
}

export function folibScannerPage (query) {
  return axios({
    url: '/api/folibScanner/page',
    method: 'get',
    params: query
  })
}
export function folibScannerDockerPage (query) {
  return axios({
    url: '/api/folibScanner/dockerPage',
    method: 'get',
    params: query
  })
}
export function folibScannerGetOne (id) {
  return axios({
    url: '/api/folibScanner/folibScannerGetOne?id='+id,
    method: 'get',
  })
}
export function getSeverity(id) {
  return axios({
    url: '/api/folibScanner/severity?id='+id,
    method: 'get'
  })
}

export function addVulnerabilitiesWhite (data) {
  return axios({
    url: '/api/configuration/folib/securityPolicy/addWhite',
    method: 'put',
    data: data
  })
}

export function removeVulnerabilitiesWhite (data) {
  return axios({
    url: '/api/configuration/folib/securityPolicy/removeWhite',
    method: 'delete',
    data: data
  })
}

export function addVulnerabilitiesBlack (data) {
  return axios({
    url: '/api/configuration/folib/securityPolicy/addBlack',
    method: 'put',
    data: data
  })
}

export function removeVulnerabilitiesBlack (data) {
  return axios({
    url: '/api/configuration/folib/securityPolicy/removeBlack',
    method: 'delete',
    data: data
  })
}

export function vulnerabilityGraph (query) {
  return axios({
    url: '/api/vulnerability/graph',
    method: 'get',
    params: query
  })
}

export function saveOrUpdateVulnerabilityNotify (data) {
  return axios({
    url: '/api/configuration/folib/securityPolicy/notify',
    method: 'put',
    data: data
  })
}

export function vulnerabilityConfig () {
  return axios({
    url: '/api/configuration/folib/securityPolicy/config',
    method: 'get',
  })
}

export function addRepositoryWhites (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/whites/'+storageId+'/'+repositoryId,
    method: 'put',
    data: obj
  })
}

export function removeRepositoryWhites (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/whites/'+storageId+'/'+repositoryId,
    method: 'delete',
    data: obj
  })
}

export function addRepositoryBlacks (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/blacks/'+storageId+'/'+repositoryId,
    method: 'put',
    data: obj
  })
}

export function removeRepositoryBlacks (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/blacks/'+storageId+'/'+repositoryId,
    method: 'delete',
    data: obj
  })
}

export function setRepositoryWhites (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/setWhites/'+storageId+'/'+repositoryId,
    method: 'put',
    data: obj
  })
}

export function setRepositoryBlacks (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/setBlacks/'+storageId+'/'+repositoryId,
    method: 'put',
    data: obj
  })
}

export function repositoryVulnerabilityStatistics (query) {
  return axios({
    url: '/api/vulnerability/repositoryVulnerabilityStatistics',
    method: 'get',
    params: query
  })
}

export function securityPolicyBlock (data) {
  return axios({
    url: '/api/configuration/folib/securityPolicy/block',
    method: 'put',
    data: data
  })
}

export function crontasksList (scope) {
  return axios({
    url: '/api/configuration/crontasks/types/list?scope='+scope,
    method: 'get'
  })
}
export function crontasksByRepository (storageId,repositoryId) {
  return axios({
    url: '/api/configuration/crontasks/getByRepository',
    method: 'get',
    params: {storageId:storageId,repositoryId:repositoryId}
  })
}

export function creatCronOne (data) {
  return axios({
    url: '/api/configuration/crontasks',
    method: 'put',
    data: data
  })
}

export function updateCronOne (data,uuid) {
  return axios({
    url: '/api/configuration/crontasks/'+uuid,
    method: 'put',
    data: data
  })
}

export function delCronOne (uuid) {
  return axios({
    url: '/api/configuration/crontasks/'+uuid,
    method: 'delete'
  })
}








