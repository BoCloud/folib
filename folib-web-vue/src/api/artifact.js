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

