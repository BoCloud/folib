// VueJS
import Vue from 'vue'

// Ant Design Vue
import Antd from 'ant-design-vue';
import 'ant-design-vue/dist/antd.css';
import { VueAxios } from './utils/request'
import store from './store/'

Vue.use(Antd);

// Photoswipe Gallery
import Photoswipe from 'vue-pswipe'
Vue.use(Photoswipe)
Vue.use(VueAxios)
// Template Layouts
import DefaultLayout from './layouts/Default.vue'
import DashboardLayout from './layouts/Dashboard.vue'
import DashboardRTLLayout from './layouts/DashboardRTL.vue'

// Adding template layouts to the vue components.
Vue.component("layout-default", DefaultLayout);
Vue.component("layout-dashboard", DashboardLayout);
Vue.component("layout-dashboard-rtl", DashboardRTLLayout);

// Main application view
import App from './App.vue'

// Vue Router
import router from './router'

// App Styling
import './scss/app.scss';

import './utils/filter' // global filter

Vue.config.productionTip = false

// 在页面加载时读取sessionStorage里的状态信息
if (sessionStorage.getItem('store')) {
  store.replaceState(
      Object.assign(
          {},
          store.state,
          JSON.parse(sessionStorage.getItem('store'))
      )
  )
}
// 在页面刷新时将vuex里的信息保存到sessionStorage里
// beforeunload事件在页面刷新时先触发
window.addEventListener('beforeunload', () => {
  sessionStorage.setItem('store', JSON.stringify(this.$store.state))
})

// Initialize Vue
new Vue({
  router,
  store,
  render: h => h(App)
}).$mount('#app')


