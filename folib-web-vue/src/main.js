// VueJS
import Vue from 'vue'

// Ant Design Vue
import Antd from 'ant-design-vue';
import 'ant-design-vue/dist/antd.css';
import { VueAxios } from './utils/request'
import store from './store/'
import './assets/font_user/iconfont.css'
import './assets/font_user/iconfont.js'
import exportExcel from './exportExcel.js'
console.log(exportExcel);

Vue.prototype.$exportExcel = exportExcel
Vue.use(Antd)

import doitUIWeb from 'doit-ui-web'
import 'doit-ui-web/lib/doituiweb.css'
Vue.use(doitUIWeb)

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

import i18n from './locale'

import Vue2OrgTree from 'vue2-org-tree'
import 'vue2-org-tree/dist/style.css'
Vue.use(Vue2OrgTree)

// Initialize Vue
new Vue({
  router,
  store,
  i18n,
  render: h => h(App)
}).$mount('#app')


