import { axios } from '@/utils/request'

export function artifactCopy (data) {
  return axios({
    url: '/api/artifact/folib/promotion/copy',
    method: 'post',
    data: data
  })
}

export function artifactMove (data) {
  return axios({
    url: '/api/artifact/folib/promotion/move',
    method: 'post',
    data: data
  })
}

export function artifactUpload (data) {
  return axios({
    url: '/api/artifact/folib/promotion/upload-files',
    method: 'post',
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