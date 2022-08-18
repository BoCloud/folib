import storage from 'store'
import { login, getInfo, logout } from '@/api/login'
import {ACCESS_TOKEN, USER_INFO} from '@/store/mutation-types'
import { welcome } from '@/utils/util'
import jwt_decode from "jwt-decode";
import router from "../../router";

const user = {
  state: {
    token: '',
    name: '',
    securityTokenKey: '',
    enabled: '',
    roles: [],
    email: ''
  },

  mutations: {
    SET_TOKEN: (state, token) => {
      state.token = token
    },
    SET_NAME: (state,  name ) => {
      state.name = name
    },
    SET_ROLES: (state, roles) => {
      state.roles = roles
    },
    SET_ENABLED: (state, enabled) => {
      state.enabled = enabled
    },
    SET_EMAIL: (state, email) => {
      state.email = email
    },
    SET_TOKEN_KEY: (state, securityTokenKey) => {
      state.securityTokenKey = securityTokenKey
    }
  },

  actions: {
    // 登录
    Login ({ commit }, userInfo) {
      return new Promise((resolve, reject) => {
        storage.remove(ACCESS_TOKEN)
        login(userInfo).then(response => {
          storage.set(ACCESS_TOKEN, response.token, jwt_decode(response.token).exp)
          commit('SET_TOKEN', response.token)
          resolve(response)
        }).catch(error => {
          reject(error)
        })
      })
    },

    GetInfo ({ commit }) {
      return new Promise((resolve, reject) => {
        getInfo().then(response => {
          const result = response
            commit('SET_ROLES', result.roles)
            commit('SET_ENABLED', result.enabled)
            commit('SET_EMAIL', result.email)
            commit('SET_NAME', result.username)
            commit('SET_TOKEN_KEY', result.securityTokenKey)

            storage.set(USER_INFO,user.state)

          resolve(response)
        }).catch(error => {
          reject(error)
        })
      })
    },
    // 登出
    Logout ({ commit, state }) {
      return new Promise((resolve) => {
        commit('SET_TOKEN', '')
        commit('SET_ROLES', [])
        storage.remove(ACCESS_TOKEN)
        storage.remove(USER_INFO)
        router.push({ name: 'login' })
      })
    }

  }
}

export default user
