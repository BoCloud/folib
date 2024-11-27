import { axios } from '@/utils/request'

export function getStorageMonitoringPage(query) {
  return axios({
    url: '/api/storageMonitoring/page',
    method: 'get',
    params: query
  })
}

export function getStorageMonitoringList(query) {
  return axios({
    url: '/api/storageMonitoring/queryStorageMonitoring',
    method: 'get',
    params: query
  })
}

export function updateStorageMonitoringData() {
  return axios({
    url: '/api/storageMonitoring/updateStorageMonitoringData',
    method: 'post',
    data: {}
  })
}