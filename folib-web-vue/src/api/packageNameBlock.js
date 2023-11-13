import { axios } from '@/utils/request'

export function getPackageNameBlock(data) {
  return axios({
    url: '/api/packageNameBlock/page',
    method: 'get',
    params: data
  })
}

export function savePackageNameBlock(data) {
  return axios({
    url: '/api/packageNameBlock',
    method: 'put',
    data: data
  })
}

export function updatePackageNameBlock(data) {
  return axios({
    url: '/api/packageNameBlock',
    method: 'post',
    data: data
  })
}

export function deletePackageNameBlock(data) {
  return axios({
    url: '/api/packageNameBlock',
    method: 'delete',
    data: data
  })
}

export function queryPackageNameBlock(data) {
  return axios({
    url: '/api/packageNameBlock/info',
    method: 'get',
    params: data
  })
}