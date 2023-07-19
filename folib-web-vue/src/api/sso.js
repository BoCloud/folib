import { axios } from '@/utils/request'


/**
 * 
 * @returns 目前支持客户端的集合
 */
export function getSsoList () {
  return axios({
    url: '/api/sso/getClients',
    method: 'get'
  })
}


/**
 * 
 * @returns 删除客户端
 */
export function deleteClient (data){
  return axios({ 
    url: '/api/sso/deleteClient/'+data.clientId,
    method: 'get',
  })
}

/**
 * 
 * @returns 新增客户端
 */
export function addSsoClient (obj) {
  return axios({
    url: '/api/sso/addClient',
    method: 'post',
    data:obj
  })
}


/**
 * 
 * @returns 更新客户端
 */
export function updateSsoClient (obj) {
  return axios({
    url: '/api/sso/updateClient',
    method: 'post',
    data:obj
  })
}







