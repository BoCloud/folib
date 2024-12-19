import { axios } from '@/utils/request'

export function getRedirect(){
    return axios({
      url: '/api/unicom/getRedirectUrl',
      method: 'get'
    })
}