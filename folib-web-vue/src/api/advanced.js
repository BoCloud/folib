import { axios } from '@/utils/request'

export function janusGraph () {
  return axios({
    url: '/api/janusGraph',
    method: 'get'
  })
}

export function deleteInstance (instanceId) {
  return axios({
    url: '/api/janusGraph/instance/' + instanceId,
    method: 'delete'
  })
}

export function reindex (data) {
  return axios({
    url: '/api/janusGraph/reindex',
    method: 'post',
    data: data
  })
}

export function registerIndex (data) {
  return axios({
    url: '/api/janusGraph/registerIndex',
    method: 'post',
    data: data
  })
}

export function getSingleDict(data) {
  return axios({
    url: '/api/dict/single',
    method: 'get',
    params: data
  })
}

export function updateSingleDict(data) {
  return axios({
    url: '/api/dict/single',
    method: 'post',
    data: data
  })
}

export function getDictList(data) {
  return axios({
    url: '/api/dict/list',
    method: 'get',
    params: data
  })
}

export function syncArtifactProvider(data) {
  return axios({
    url: '/api/artifact/syncArtifactProvider',
    method: 'post',
    data: data
  })
}

export function folderList(data) {
  return axios({
    url: '/api/folder/list',
    method: 'get',
    params: data
  })
}

export function saveBackup(data) {
  return axios({
    url: '/api/backup',
    method: 'post',
    data: data
  })
}

export function deleteDict(data) {
  return axios({
    url: '/api/dict/delete',
    method: 'delete',
    params: data
  })
}


export function systemDict(data) {
  return axios({
    url: '/api/dict/system',
    method: 'get',
    params: data
  })
}
