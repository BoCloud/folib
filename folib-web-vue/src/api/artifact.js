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
    url: '/api/artifact/artifactMetadata',
    method: 'delete',
    data: data
  })
}


export function rpmArtifactUpload (storageId,repositoryId,obj) {
  return axios({
    url: '/storages/'+storageId+'/'+repositoryId+'/Packages',
    method: 'put',
    headers: {
      'Content-Type': "multipart/form-data",
    },
    data: obj
  })
}
