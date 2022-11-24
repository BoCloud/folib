import axios from 'axios'
import store from '@/store'
import storage from 'store'
import notification from 'ant-design-vue/es/notification'
import { VueAxios } from './axios'
import { ACCESS_TOKEN } from '@/store/mutation-types'
import router from "../router";
import Cookies from 'js-cookie'

// 创建 axios 实例
const request = axios.create({
  // API 请求的默认前缀
  timeout: 30000 // 请求超时时间
})

// 异常拦截处理器
const errorHandler = (error) => {
  if (error.response) {
    const data = error.response.data
    // 从 localstorage 获取 token
    const token = storage.get(ACCESS_TOKEN)
    if (error.response.status === 403) {
      notification.error({
        message: '没有权限操作',
        description: data.message
      })
    }
    if (error.response.status === 401) {
      let message = error.response.data.error
      if (message.indexOf('invalid.credentials') !== -1) {
        setTimeout(() => {
          notification.error({
            message: '提示',
            description: '账号或密码错误'
          })
        }, 100)
      } else if (message.indexOf('User account is locked') !== -1) {
        setTimeout(() => {
          notification.error({
            message: '提示',
            description: '登录失败，用户未激活'
          })
        }, 100)
      } else {
        setTimeout(() => {
          notification.error({
            message: '权限信息',
            description: '登录信息已过期将为你转跳至登录页面'
          })
        }, 100)
      }
      store.dispatch('Logout').then(() => {
        window.location.reload()
      })

      if (token) {

      } else {


      }
    }
  }
  return Promise.reject(error)
}

// request interceptor
request.interceptors.request.use(config => {
  if (Cookies.get("access_token")) {
    Cookies.remove("access_token")
  }


  const token = storage.get(ACCESS_TOKEN) ? storage.get(ACCESS_TOKEN) : Cookies.get("access_token")
  // console.log(config)
  // 如果 token 存在
  // 让每个请求携带自定义 token 请根据实际情况自行修改
  if (token) {
    config.headers[ACCESS_TOKEN] = "Bearer " + token
  }
  return config
}, errorHandler)

// response interceptor
request.interceptors.response.use((response) => {
  return response.data
}, errorHandler)

const installer = {
  vm: {},
  install(Vue) {
    Vue.use(VueAxios, request)
  }
}

export default request

export {
  installer as VueAxios,
  request as axios
}
