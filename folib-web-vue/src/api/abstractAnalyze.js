import { axios } from '@/utils/request'

/**
 *
 * @returns 获取配置信息
 */
export function getAnalyzeConfig (){
    return axios({
        url: '/api/sca/getAnalyzeConfig',
        method: 'get'
    })
}

/**
 *
 * @returns 获取缓存配置
 */
export function getCacheAnalyzeConfig (){
    const foEyesConfig = localStorage.getItem("ABSTRACT_ANALYZE_CONFIG")
    if (foEyesConfig) {
        return JSON.parse(foEyesConfig)
    }
    return null
}