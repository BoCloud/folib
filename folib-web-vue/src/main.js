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

// Initialize Vue
new Vue({
  router,
  store,
  render: h => h(App)
}).$mount('#app')
