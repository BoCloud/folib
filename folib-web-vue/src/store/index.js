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
    currentTreeNode: {},
    isChecked: false,
    switchDisabled: true
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
    // 切换制品仓库页面展示模式
    setIsChecked(state, key) {
      state.isChecked = key
      console.log(state.isChecked, 'state.setIsChecked')
    },
    // 控制顶部切换模式按钮不可点击，需等列表接口加载完之后才可点击
    setSwitchDisabled(state, key) {
      state.switchDisabled = key
      console.log(state.switchDisabled, 'state.switchDisabled')
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

