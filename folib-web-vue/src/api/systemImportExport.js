import { axios } from '@/utils/request'

export function systemExport (data) {
  return axios({
    url: '/api/systemConfiguration/export',
    method: 'post',
    data: data
  })
}

export function systemImport (data) {
  return axios({
    url: '/api/systemConfiguration/import',
    method: 'post',
    data: data
  })
}