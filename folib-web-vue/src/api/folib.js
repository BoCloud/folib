import { axios } from '@/utils/request'


export function getStorages () {
  return axios({
    url: '/api/configuration/folib/storages',
    method: 'get'
  })
}

export function queryStorages (dataQuery) {
  return axios({
    url: `/api/configuration/folib/storages/queryStorages?page=${dataQuery.page}&limit=${dataQuery.limit}`,
    method: 'get'
  })
}

export function getLibrary (libId) {
  return axios({
    url: '/api/configuration/folib/storages/'+libId,
    method: 'get'
  })
}

export function getLibraryFilter (libId) {
  return axios({
    url: '/api/configuration/folib/storages/'+libId + '?filter=true',
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

export function queryRepositories (dataQuery) {
  return axios({
    url: `/api/configuration/folib/storages/queryStoragesAndRepositories?page=${dataQuery.page}&limit=${dataQuery.limit}`,
    method: 'get'
  })
}
export function getPermissionStoragesAndRepositories (dataQuery) {
  return axios({
    url: '/api/configuration/folib/storages/getPermissionStoragesAndRepositories',
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

export function aliveRepository (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/'+storageId+'/'+repositoryId+'/alive',
    method: 'post',
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
export function getArtifact (type,storageId,repositoryId,path, digest){
  if (!digest) {
    digest = ''
  }
  return axios({
    url: '/api/browse/getArtifact/'+storageId+'/'+repositoryId+'/'+path+'?type='+type + '&digest=' + digest + '&report=' + false,
    method: 'get'
  })
}

export function getArtifactReport (type,storageId,repositoryId,path, digest){
  if (!digest) {
    digest = ''
  }
  return axios({
    url: '/api/browse/getArtifact/'+storageId+'/'+repositoryId+'/'+path+'?type='+type + '&digest=' + digest + '&report=' + true,
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

export function previewArtifact (storageId,repositoryId,path){
  return axios({
    url: '/api/artifact/preview/'+storageId+'/'+repositoryId+'/'+path,
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

export function queryOnScanTree (data) {
  return axios({
    url: '/api/scanRules/queryOnScanTree',
    method: 'get'
  })
}

export function queryBomOnScanTree (data) {
  return axios({
    url: '/api/scanRules/queryBomOnScanTree',
    method: 'get'
  })
}

export function getCount () {
  return axios({
    url: '/api/scanner/getCount',
    method: 'get'
  })
}

export function getScannerSumDifVoList () {
  return axios({
    url: '/api/scanner/repositories',
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
    url: '/api/scanner/weekCount',
    method: 'get'
  })
}

export function mounthDayCount () {
  return axios({
    url: '/api/scanner/monthCount',
    method: 'get'
  })
}

export function scannerRepositoryPage (query) {
  return axios({
    url: '/api/scanner/repository',
    method: 'get',
    params: query
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

export function securityPolicyConfig () {
  return axios({
    url: '/api/configuration/folib/securityPolicy/config',
    method: 'get',
  })
}

export function addRepositoryWhites (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/'+ storageId + '/'+ repositoryId + '/whites',
    method: 'put',
    data: obj
  })
}

export function removeRepositoryWhites (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/'+ storageId + '/'+ repositoryId + '/whites',
    method: 'delete',
    data: obj
  })
}

export function addRepositoryBlacks (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/' + storageId + '/' + repositoryId + '/blacks',
    method: 'put',
    data: obj
  })
}

export function removeRepositoryBlacks (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/' + storageId + '/' + repositoryId + '/blacks',
    method: 'delete',
    data: obj
  })
}

export function setRepositoryWhites (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/' + storageId + '/' + repositoryId + '/setWhites',
    method: 'put',
    data: obj
  })
}

export function setRepositoryBlacks (storageId,repositoryId,obj) {
  return axios({
    url: '/api/configuration/folib/storages/' + storageId + '/' + repositoryId + '/setBlacks',
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

export function securityPolicyAddPackageName (data) {
  return axios({
    url: '/api/configuration/folib/securityPolicy/packageName',
    method: 'put',
    data: data
  })
}

export function securityPolicyDeletePackageName (data) {
  return axios({
    url: '/api/configuration/folib/securityPolicy/packageName',
    method: 'delete',
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
export function getArtifactDispatchStoragesAndRepositories (dataQuery) {
  return axios({
    url: '/api/configuration/folib/storages/getDispatchStoragesAndRepositories',
    method: 'get',
    params: dataQuery
  })
}

export function repositoryPermission (storageId, repositoryId, data) {
  return axios({
    url: '/api/configuration/folib/storages/'+storageId+'/'+repositoryId + '/permission',
    method: 'post',
    data: data
  })
}

export function repositoryEnableUsers (query) {
  return axios({
    url: '/api/configuration/folib/storages/repositoryEnableUsers',
    method: 'get',
    params: query
  })
}

export function getRepositoryPermission (query) {
  return axios({
    url: '/api/configuration/folib/storages/repositoryPermission',
    method: 'get',
    params: query
  })
}

export function deleteRepositoryPermission (query) {
  return axios({
    url: '/api/configuration/folib/storages/repositoryPermission',
    method: 'delete',
    params: query
  })
}

export function getStorageAndRepositoryPermission (storageId,repositoryId) {
  return axios({
    url: '/api/account/permission/'+storageId+'/'+repositoryId,
    method: 'get'
  })
}

export function unionRepositoryConfig (storageId,repositoryId,data) {
  return axios({
    url: '/api/configuration/folib/storages/'+storageId+'/'+repositoryId+'/unionRepository',
    method: 'put',
    data: data
  })
}

export function getFoEyesEnable () {
  return axios({
    url: '/api/scanner/foEyesEnable',
    method: 'get'
  })
}

export function getArtifactsPage (query) {
  return axios({
    url: '/api/artifact/getArtifacts',
    method: 'get',
    params: query
  })
}

export function queryRepositoriesByStorage (dataQuery) {
  return axios({
    url: '/api/configuration/folib/storages/queryStoragesAndRepositories',
    params: dataQuery,
    method: 'get'
  })
}