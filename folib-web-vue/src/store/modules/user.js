import storage from 'store'
import { login, getInfo, logout } from '@/api/login'
import {ACCESS_TOKEN, USER_INFO} from '@/store/mutation-types'
import jwt_decode from "jwt-decode";
import router from "../../router";
import store from '@/store'

const user = {
  state: {
    token: '',
    name: '',
    securityTokenKey: '',
    enabled: '',
    roles: [],
    authorities: [],
    email: '',
    avatar: ''
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
    SET_AUTHORITIES: (state, authorities) => {
      state.authorities = authorities
    },
    SET_ENABLED: (state, enabled) => {
      state.enabled = enabled
    },
    SET_EMAIL: (state, email) => {
      state.email = email
    },
    SET_TOKEN_KEY: (state, securityTokenKey) => {
      state.securityTokenKey = securityTokenKey
    },
    SET_AVATAR: (state, avatar) => {
      state.avatar = avatar
    },
  },

  actions: {
    // 登录
    Login ({ commit }, userInfo) {
      return new Promise((resolve, reject) => {
        storage.remove(ACCESS_TOKEN)
        login(userInfo).then(response => {
         // console.log(jwt_decode(response.token).exp)
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
            commit('SET_AUTHORITIES', result.authorities)
            commit('SET_ENABLED', result.enabled)
            commit('SET_EMAIL', result.email)
            commit('SET_NAME', result.username)
            commit('SET_TOKEN_KEY', result.securityTokenKey)
            commit('SET_AVATAR', result.avatar)


            // console.log(store.state)
            // storage.set(USER_INFO,user.state)
          //

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
        commit('SET_AUTHORITIES', [])
        commit('SET_ENABLED', '')
        commit('SET_EMAIL', '')
        commit('SET_NAME', '')
        commit('SET_TOKEN_KEY', '')
        commit('SET_AVATAR', '')
        storage.remove(ACCESS_TOKEN)
        storage.remove(USER_INFO)
        storage.remove("libView_repository")
        router.push({ name: 'login' })
      })
    }

  }
}

export default user
