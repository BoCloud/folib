<template>
  <a-layout-sider
    collapsible
    class="sider-primary"
    breakpoint="lg"
    collapsed-width="0"
    width="250px"
    ref="scrollContainer"
    :collapsed="sidebarCollapsed"
    @collapse="$emit('toggleSidebar', !sidebarCollapsed)"
    :trigger="null"
    :class="['ant-layout-sider-' + sidebarColor, 'ant-layout-sider-' + sidebarTheme]"
    theme="light"
    :style="{ backgroundColor: 'transparent' }"
  >
    <div class="brand"><img src="images/folib/foliblogo.svg" alt="foliblogo" /> <span>{{ instanceName }}</span></div>
    <hr class="gradient-line"/>
    <!-- Sidebar Navigation Menu -->
     <div class="left-menu_list">
       <a-menu theme="light" mode="inline" :open-keys="openKeys" @openChange="onOpenChange">
         <a-menu-item class="menu-item-header"> {{ $t('Sidebars.ProductManagement') }} </a-menu-item>
         <a-menu-item>
           <router-link :to="'/storages/home'">
             <span class="icon">
               <a-icon type="appstore" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.ProductWarehouse') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="userInfo.token">
           <router-link to="/storage/scanner">
             <span class="icon">
               <a-icon type="read" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.SecurityScanning') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="(this.userInfo.roles.indexOf('ADMIN') > -1 || this.userInfo.roles.indexOf('OPEN_SOURCE_MANAGE') > -1 ) && analyzeEnable" class="menu-item-header">
           <hr class="mt-5" />
           {{ $t('Sidebars.OpenSourceGovernance') }}
         </a-menu-item>
         <a-menu-item v-if="this.userInfo.roles.indexOf('ADMIN') > -1 || this.userInfo.roles.indexOf('OPEN_SOURCE_MANAGE') > -1">
           <router-link to="/artifacts">
             <span class="icon">
               <a-icon type="profile" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.ProductAnalysis') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="this.userInfo.roles.indexOf('ADMIN') > -1 || this.userInfo.roles.indexOf('OPEN_SOURCE_MANAGE') > -1">
           <router-link to="/components">
             <span class="icon">
               <a-icon type="hdd" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.OpenSourceComponents') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="this.userInfo.roles.indexOf('ADMIN') > -1 || this.userInfo.roles.indexOf('OPEN_SOURCE_MANAGE') > -1">
           <router-link to="/vulnerabilities">
             <span class="icon">
               <a-icon type="alert" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.VulnerabilityDatabase') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="this.userInfo.roles.indexOf('ADMIN') > -1 || this.userInfo.roles.indexOf('OPEN_SOURCE_MANAGE') > -1">
           <router-link to="/licenses">
             <span class="icon">
               <a-icon type="file-text" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.CertificateStore') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="userInfo.roles.indexOf('ADMIN') > -1" class="menu-item-header">
           <hr class="mt-5" />
           {{ $t('Sidebars.SetupManagement') }}
         </a-menu-item>
         <a-menu-item v-if="userInfo.roles.indexOf('ADMIN') > -1">
           <router-link to="/users">
             <span class="icon">
               <a-icon type="smile" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.UserManagement') }}</span>
           </router-link>
         </a-menu-item>
          <a-menu-item v-if="userInfo.roles.indexOf('ADMIN') > -1" >
           <router-link to="/permissions">
             <span class="icon">
               <a-icon type="smile" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.Permissions') }}</span>
           </router-link>
         </a-menu-item>

         <a-menu-item v-if="userInfo.roles.indexOf('ADMIN') > -1">
           <router-link to="/accessToken">
             <span class="icon">
               <a-icon type="lock" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.AccessToken') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="userInfo.roles.indexOf('ADMIN') > -1">
           <router-link to="/settings">
             <span class="icon">
               <a-icon type="tool" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.GlobalSettings') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="userInfo.roles.indexOf('ADMIN') > -1">
           <router-link to="/monitor">
             <span class="icon">
               <a-icon type="fund" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.HealthMonitoring') }}</span>
           </router-link>
         </a-menu-item>
         <a-menu-item v-if="userInfo.roles.indexOf('ADMIN') > -1">
           <router-link to="/advanced">
             <span class="icon">
               <a-icon type="control" theme="filled" class="m-0" />
             </span>
             <span class="label">{{ $t('Sidebars.SeniorOperations') }}</span>
           </router-link>
         </a-menu-item>
       </a-menu>
      <!-- / Sidebar Navigation Menu -->

      <!-- Sidebar Footer -->
      <div class="aside-footer">
        <div class="footer-box">
          <span class="icon">
            <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path
                d="M3 4C3 3.44772 3.44772 3 4 3H16C16.5523 3 17 3.44772 17 4V6C17 6.55228 16.5523 7 16 7H4C3.44772 7 3 6.55228 3 6V4Z"
                fill="#111827"
              />
              <path
                d="M3 10C3 9.44771 3.44772 9 4 9H10C10.5523 9 11 9.44771 11 10V16C11 16.5523 10.5523 17 10 17H4C3.44772 17 3 16.5523 3 16V10Z"
                fill="#111827"
              />
              <path
                d="M14 9C13.4477 9 13 9.44771 13 10V16C13 16.5523 13.4477 17 14 17H16C16.5523 17 17 16.5523 17 16V10C17 9.44771 16.5523 9 16 9H14Z"
                fill="#111827"
              />
            </svg>
          </span>
          <h6>{{ $t('Sidebars.DoNotKnowHowToUseIt') }}</h6>
          <p>{{ $t('Sidebars.CheckTheHelpDocumentation') }}</p>
          <a-button block size="small" href="/help/index.html" target="_blank" rel="noopenner noreferrer"> {{ $t('Sidebars.userManual') }} </a-button>
        </div>
    </div>
  </div>
    <!-- / Sidebar Footer -->
  </a-layout-sider>
  <!-- / Main Sidebar -->
</template>

<script>
import store from "@/store";
import { hasRole, isAdmin, isAnonymous, isLogin } from "@/utils/permission";
import {
  getCacheConfig
} from "@/api/foEyes";
import {getCacheAnalyzeConfig} from "@/api/abstractAnalyze";
export default {
  props: {
    // Sidebar collapsed status.
    sidebarCollapsed: {
      type: Boolean,
      default: false,
    },

    // Main sidebar color.
    sidebarColor: {
      type: String,
      default: "primary",
    },

    // Main sidebar theme : light, white, dark.
    sidebarTheme: {
      type: String,
      default: "light",
    },
  },
  data() {
    return {
      rootSubmenuKeys: ["dashboards", "pages", "applications", "ecommerce", "authentication", "basic", "components", "changelog"],
      openKeys: this.$route.meta.sidebarMap,
      userInfo: {},
      instanceName:sessionStorage.getItem("instanceName")||"",
      foeyesEnable: false,
      analyzeEnable: false,
    };
  },
  created() {
    this.userInfo = store.state.user;
    this.getFoEyesEnable()
  },
  methods: {
    onOpenChange(openKeys) {
      const latestOpenKey = openKeys.find((key) => this.openKeys.indexOf(key) === -1);

      if (this.rootSubmenuKeys.indexOf(latestOpenKey) === -1) {
        this.openKeys = openKeys;
      } else {
        this.openKeys = latestOpenKey ? [latestOpenKey] : [];
      }
      this.getFoEyesEnable()
      this.getAnalyzeEnable()
    },
    getFoEyesEnable() {
      const cacheConfig = getCacheConfig()
      if (cacheConfig) {
        this.foeyesEnable = cacheConfig.enable
      }
    },
    getAnalyzeEnable  (){
        const cacheConfig = getCacheAnalyzeConfig()
        if (cacheConfig) {
            this.analyzeEnable = cacheConfig.enable
        }
    }
  },
};
</script>
<style lang="scss">
  .sider-primary{
    margin-top:10px !important;
    height: calc(100vh - 30px) !important;
    overflow: hidden !important;

    .ant-layout-sider-children{
      padding-right: 10px !important;
    }

    .left-menu_list{
      padding-right: 10px;
      overflow-x:hidden;
      overflow-y: hidden;
      height: calc(100vh - 120px);

      &:hover{
        overflow-y: auto;
        .aside-footer{
          padding-right: 0px !important;
        }
      }

      .aside-footer{
        padding-bottom: 0 !important;
        padding-right: 5px !important;
      }
    }
  }
  .layout-dashboard.sidebar-minimized.has-sidebar .ant-layout-sider.sider-primary:not(:hover) .ant-menu-item a{
    margin-left:23px !important;
  }
  .layout-dashboard.sidebar-minimized.has-sidebar .ant-layout-sider.sider-primary:not(:hover) .label{
    display: none !important;
  }
</style>
