import { axios } from '@/utils/request'

export function queryCustomLayout(data) {
  return axios({
    url: '/api/customLayout/page',
    method: 'get',
    params: data
  })
}

export function queryCustomLayoutList(data) {
  return axios({
    url: '/api/customLayout/list',
    method: 'get',
    params: data
  })
}

export function saveCustomLayout(data) {
  return axios({
    url: '/api/customLayout',
    method: 'put',
    data: data
  })
}

export function updateCustomLayout(data) {
  return axios({
    url: '/api/customLayout',
    method: 'post',
    data: data
  })
}

export function getCustomLayout(data) {
  return axios({
    url: '/api/customLayout/info',
    method: 'get',
    params: data
  })
}

export function deleteCustomLayout(data) {
  return axios({
    url: '/api/customLayout',
    method: 'delete',
    data: data
  })
}