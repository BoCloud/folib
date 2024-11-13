import Vue from 'vue'
import Vuex from 'vuex'

import user from './modules/user'
import language from './modules/language'
import Cookies from 'js-cookie'
import getters from './getters'

Vue.use(Vuex)
const store = new Vuex.Store({
  modules: {
    user,
    language
  },
  state: {
    newDetailPage: false,
    currentTreeNode: {}
  },
  mutations: {
    setNewDetailPage(state, key) {
      state.newDetailPage = key
      console.log(state.newDetailPage, 'state.newDetailPage')
    },
    setCurrentTreeNode(state, info) {
      state.currentTreeNode = info
      console.log(state.currentTreeNode, 'state.currentTreeNode')
    },
  },
  actions: {},
  getters
})

Vue.nextTick(() => {
  const token = Cookies.get("token");
  if (token) {
    store.dispatch("Token", token);
    store.dispatch("GetInfo").then((res) => {
    })
  }
})
export default store

