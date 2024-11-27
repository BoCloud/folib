import { axios } from '@/utils/request'

/**
 * 
 * @returns 获取配置信息
 */
export function getConfig (){
  return axios({
    url: '/api/foEyes/getConfig',
    method: 'get'
  })
}

/**
 * 
 * @returns 获取缓存配置
 */
export function getCacheConfig (){
  const foEyesConfig = localStorage.getItem("FOEYES_CONFIG")
  if (foEyesConfig) {
    return JSON.parse(foEyesConfig)
  }
  return null
}

/**
 * 
 * @returns 查询项目信息
 */
export function getProjectInfo (storageId,repositoryId,path){
  return axios({
    url: '/api/foEyes/'+storageId+'/'+repositoryId+'/'+path,
    method: 'get'
  })
}