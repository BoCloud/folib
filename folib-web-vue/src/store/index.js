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
  state: {
    newDetailPage: false,
    currentTreeNode:{}
  },
  mutations: {
    setNewDetailPage(state, key) {
      state.newDetailPage = key
      console.log(state.newDetailPage,'state.newDetailPage')
    },
    setCurrentTreeNode(state, info) {
      state.currentTreeNode = info
      console.log(state.currentTreeNode,'state.currentTreeNode')
    },
  },
  actions: {},
  getters
})
