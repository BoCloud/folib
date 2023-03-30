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