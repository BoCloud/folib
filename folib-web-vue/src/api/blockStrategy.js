import { axios } from '@/utils/request'

export function queryBlockStrategy(data) {
  return axios({
    url: '/api/blockStrategy/page',
    method: 'get',
    params: data
  })
}

export function saveBlockStrategy(data) {
  return axios({
    url: '/api/blockStrategy',
    method: 'put',
    data: data
  })
}

export function updateBlockStrategy(data) {
  return axios({
    url: '/api/blockStrategy',
    method: 'post',
    data: data
  })
}

export function getBlockStrategy(data) {
  return axios({
    url: '/api/blockStrategy/info',
    method: 'get',
    params: data
  })
}

export function deleteBlockStrategy(data) {
  return axios({
    url: '/api/blockStrategy',
    method: 'delete',
    data: data
  })
}