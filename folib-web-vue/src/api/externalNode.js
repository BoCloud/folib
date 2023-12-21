import { axios } from '@/utils/request'

export function getExternalNodeList(query) {
  return axios({
    url: '/api/externalNode/page',
    method: 'get',
    params: query
  })
}

export function getExternalNodeInfo(id) {
  return axios({
    url: '/api/externalNode/' + id,
    method: 'get'
  })
}

export function saveExternalNode(data) {
  return axios({
    url: '/api/externalNode',
    method: 'put',
    data: data
  })
}

export function updateExternalNode(data) {
  return axios({
    url: '/api/externalNode',
    method: 'post',
    data: data
  })
}

export function deleteExternalNode(id) {
  return axios({
    url: '/api/externalNode/' + id,
    method: 'delete',
  })
}

export function getExternalNodeRepositories(query) {
  return axios({
    url: '/api/externalNode/repositories',
    method: 'get',
    params: query
  })
}