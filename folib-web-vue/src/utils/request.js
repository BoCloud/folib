import axios from "axios";
import store from "@/store";
import storage from "store";
import notification from "ant-design-vue/es/notification";
import { VueAxios } from "./axios";
import { ACCESS_TOKEN } from "@/store/mutation-types";
import router from "../router";
import Cookies from "js-cookie";

// 创建 axios 实例
const request = axios.create({
  // API 请求的默认前缀
  timeout: 30000, // 请求超时时间
});

let messageContrl = {
  status : false
}
// 异常拦截处理器
const errorHandler = (error) => {
  if (error.response) {
    const data = error.response.data;
    // 从 localstorage 获取 token
    const token = storage.get(ACCESS_TOKEN);
    if (error.response.status === 403) {
      messageCtl(messageContrl, "error", "没有权限操作", data.message)
    }
    if (error.response.status === 304) {
      messageCtl(messageContrl, "error", "操作失败", data.message)
    }
    if (error.response.status === 401) {
      let message = error.response.data.error
      if (message.indexOf("invalid.credentials") !== -1) {
        messageCtl(messageContrl, "error", "提示", "账号或密码错误")
      } else if (message.indexOf("User account is locked") !== -1) {
        messageCtl(messageContrl, "error", "提示", "登录失败，用户未激活")
      } else {
        messageCtl(messageContrl, "warning", "提示", "登录信息已过期或未登录")
      }
      store.dispatch("Logout").then(() => {   
      });
    }
  }
  return Promise.reject(error);
};

// request interceptor
request.interceptors.request.use((config) => {
  if (Cookies.get("access_token")) {
    Cookies.remove("access_token");
  }

  const token = storage.get(ACCESS_TOKEN) ? storage.get(ACCESS_TOKEN) : Cookies.get("access_token");
  // console.log(config)
  // 如果 token 存在
  // 让每个请求携带自定义 token 请根据实际情况自行修改
  if (token) {
    const hasWordDependency = config.url.includes("/dependency");
    // 有/dependency
    if (hasWordDependency) {
      const foEyesConfig = localStorage.getItem("FOEYES_CONFIG")
      if (foEyesConfig) {
        config.headers["X-Api-Key"] = JSON.parse(foEyesConfig).accessKey
      }
    } else {
      config.headers[ACCESS_TOKEN] = "Bearer " + token;
    }
  }
  return config;
}, errorHandler);

// response interceptor
request.interceptors.response.use((response) => {
  const hasWordApi = response.config.url.includes("/api");
  // 有/api
  if (hasWordApi) {
    return response.data;
  } else {
    return response;
  }
}, errorHandler);

const installer = {
  vm: {},
  install(Vue) {
    Vue.use(VueAxios, request);
  },
};

// 防止出现多个提示框的解决办法
export function messageCtl(messageContrl, type, message, description) {
  console.log("messageContrl ", messageContrl.status)
  if(!messageContrl.status){
    messageContrl.status = true
    setTimeout(() => {
      notification[type]({
        message: message,
        description: description,
      })
    }, 100)
    setTimeout(() => {
      messageContrl.status = false
    }, 5000)
  }
}

export default request;

export { installer as VueAxios, request as axios };
