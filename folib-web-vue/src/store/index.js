import Vue from 'vue'
import Vuex from 'vuex'

import user from './modules/user'
import language from './modules/language'

import getters from './getters'

Vue.use(Vuex)

export default new Vuex.Store({
  modules: {
    user,
    language
  },
  state: {},
  mutations: {},
  actions: {},
  getters
})
