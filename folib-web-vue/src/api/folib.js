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
export function folibScannerGetOne (id) {
  return axios({
    url: '/api/folibScanner/folibScannerGetOne?id='+id,
    method: 'get',
  })
}






