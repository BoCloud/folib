import { axios } from '@/utils/request'

export function artifactCopy (data) {
  return axios({
    url: '/api/artifact/folib/promotion/copy',
    method: 'post',
    timeout: 15 * 60 * 1000,
    data: data
  })
}

export function artifactMove (data) {
  return axios({
    url: '/api/artifact/folib/promotion/move',
    method: 'post',
    timeout: 15 * 60 * 1000,
    data: data
  })
}

export function artifactUpload (data) {
  return axios({
    url: '/api/artifact/folib/promotion/upload-files',
    method: 'post',
    timeout: 15 * 60 * 1000,
    headers: {"Content-type": "multipart/form-data",},
    data: data
  })
}

export function singleArtifactUpload (data) {
  return axios({
    url: '/api/artifact/folib/promotion/upload',
    method: 'post',
    headers: {"Content-type": "multipart/form-data",},
    data: data
  })
}

export function parseArtifact (data) {
  return axios({
    url: '/api/artifact/folib/promotion/parseArtifact',
    method: 'post',
    timeout: 15 * 60 * 1000,
    headers: {"Content-type": "multipart/form-data",},
    data: data
  })
}

export function artifactUploadProgress (data, uuid, fileName) {
  return axios({
    url: '/api/artifact/folib/promotion/upload-files?uuid=' + uuid + '&fileName=' + fileName,
    method: 'post',
    timeout: 15 * 60 * 1000,
    headers: {"Content-type": "multipart/form-data",},
    data: data
  })
}

export function queryArtifactUploadProcess (uuid) {
  return axios({
    url: '/api/artifact/folib/promotion/uploadProcess?dictType=upload_process&uuid=' + uuid,
    method: 'get'
  })
}

export function deleteArtifactUploadProcess (uuid) {
  return axios({
    url: '/api/artifact/folib/promotion/uploadProcess?dictType=upload_process&uuid=' + uuid,
    method: 'delete'
  })
}

export function saveArtifactMetadata (data) {
  return axios({
    url: '/api/artifact/artifactMetadata',
    method: 'put',
    data: data
  })
}

export function updateArtifactMetadata (data) {
  return axios({
    url: '/api/artifact/artifactMetadata',
    method: 'post',
    data: data
  })
}

export function deleteArtifactMetadata (data) {
  return axios({
    url: '/api/artifact/deleteArtifactMetadata',
    method: 'post',
    data: data
  })
}


export function rpmArtifactUpload (storageId,repositoryId,obj,uuid,fileName) {
  return axios({
    url: '/storages/'+storageId+'/'+repositoryId+'/Packages?uuid=' + uuid + '&fileName=' + fileName,
    method: 'post',
    timeout: 15 * 60 * 1000,
    headers: {
      'Content-Type': "multipart/form-data",
    },
    data: obj
  })
}

export function artifactDispatch (data) {
  return axios({
    url: '/api/artifact/folib/promotion/artifactDispatch',
    method: 'post',
    data: data
  })
}

export function buildGraphIndex (data) {
  return axios({
    url: '/api/artifact/buildGraphIndex',
    method: 'post',
    params: data
  })
}

export function artifactUploadZip (data, uuid, fileName) {
  return axios({
    url: '/api/artifact/store?uuid=' + uuid + '&fileName=' + fileName,
    method: 'post',
    timeout: 15 * 60 * 1000,
    headers: {"Content-type": "multipart/form-data",},
    data: data
  })
}

export function conanInfo (data) {
  return axios({
    url: '/api/conan/info',
    method: 'post',
    data: data
  })
}

export function conanPackageInfo (data) {
  return axios({
    url: '/api/conan/packageInfo',
    method: 'post',
    data: data
  })
}

export function getLayouts () {
  return axios({
    url: '/api/artifact/getLayouts',
    method: 'get',
  })
}

export function retryNodeOption (syncNo) {
  return axios({
    url: '/api/artifact/folib/promotion/retryNodeOption/'+syncNo,
    method: 'post'
  })
}

export function retryArtifactDispatch (syncNo,type) {
  return axios({
    url: '/api/artifact/folib/promotion/retryArtifactDispatch/'+syncNo+'/'+type,
    method: 'post'
  })
}

export function updateTaskQueuePriority (syncNo,priority) {
  return axios({
    url: '/api/artifact/folib/promotion/updateTaskQueuePriority/'+syncNo+'/'+priority,
    method: 'post'
  })
}

export function getRawPathSize (storageId,repositoryId,path) {
  return axios({
    url: '/api/artifact/rawPathSize/'+storageId+'/'+repositoryId+'/'+path,
    method: 'get'
  })
}
export function deleteTask (syncNo,priority) {
  return axios({
    url: '/api/artifact/folib/promotion/deleteTask/'+syncNo,
    method: 'delete'
  })
}
