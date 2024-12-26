import { axios } from '@/utils/request'

export function queryBackupStrategy(data) {
  return axios({
    url: '/api/backupStrategy/page',
    method: 'get',
    params: data
  })
}

export function saveBackupStrategy(data) {
  return axios({
    url: '/api/backupStrategy',
    method: 'put',
    data: data
  })
}

export function updateBackupStrategy(data) {
  return axios({
    url: '/api/backupStrategy',
    method: 'post',
    data: data
  })
}

export function getBackupStrategy(data) {
  return axios({
    url: '/api/backupStrategy/info',
    method: 'get',
    params: data
  })
}

export function deleteBackupStrategy(data) {
  return axios({
    url: '/api/backupStrategy',
    method: 'delete',
    data: data
  })
}

export function executeBackup(data) {
  return axios({
    url: '/api/backupStrategy/executeBackup',
    method: 'post',
    data: data
  })
}