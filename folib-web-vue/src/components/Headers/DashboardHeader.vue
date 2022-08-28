<template>
  <!-- Main Sidebar -->
  <component :is="navbarFixed ? 'a-affix' : 'div'" :offset-top="top">
    <!-- Layout Header -->
    <a-layout-header>
      <a-row type="flex">
        <!-- Header Breadcrumbs & Title Column -->
        <a-col :span="20" :md="6">
          <!-- Header Breadcrumbs -->
          <a-breadcrumb>
            <template v-for="(item, key) in $route.meta.breadcrumbs">
              <a-breadcrumb-item
                  v-if="key == $route.meta.breadcrumbs.length - 1"
                  :key="key"
              >{{ item }}
              </a-breadcrumb-item
              >
              <a-breadcrumb-item v-else :key="key"
              >
                <router-link to="/">{{ item }}</router-link>
              </a-breadcrumb-item
              >
            </template>
          </a-breadcrumb>
          <!-- / Header Breadcrumbs -->

          <!-- Header Page Title -->
          <div class="ant-page-header-heading">
            <span class="ant-page-header-heading-title">{{
                this.$route.meta.title || this.$route.name
              }}</span>
          </div>
          <!-- / Header Page Title -->
        </a-col>
        <!-- / Header Breadcrumbs & Title Column -->

        <!-- Header Breadcrumbs & Title Column -->
        <a-col :span="4" :md="1" class="sidebar-toggler-col">
          <!-- / Header Page Title -->
          <a-button
              type="link"
              class="sidebar-toggler"
              @click="$emit('minimizeSidebar'), resizeEventHandler()"
          >
            <svg
                width="20"
                height="20"
                xmlns="http://www.w3.org/2000/svg"
                viewBox="0 0 448 512"
            >
              <path
                  d="M16 132h416c8.837 0 16-7.163 16-16V76c0-8.837-7.163-16-16-16H16C7.163 60 0 67.163 0 76v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16z"
              />
            </svg>
          </a-button>
        </a-col>
        <!-- / Header Breadcrumbs & Title Column -->

        <!-- Header Control Column -->
        <a-col :span="24" :md="17" class="header-control">
          <!-- Header Control Buttons -->

          <a-button
              type="link"
              ref="secondarySidebarTriggerBtn"
              @click="$emit('toggleSettingsDrawer', true)"
          >
            <svg
                width="20"
                height="20"
                viewBox="0 0 20 20"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
            >
              <path
                  fill-rule="evenodd"
                  clip-rule="evenodd"
                  d="M11.4892 3.17094C11.1102 1.60969 8.8898 1.60969 8.51078 3.17094C8.26594 4.17949 7.11045 4.65811 6.22416 4.11809C4.85218 3.28212 3.28212 4.85218 4.11809 6.22416C4.65811 7.11045 4.17949 8.26593 3.17094 8.51078C1.60969 8.8898 1.60969 11.1102 3.17094 11.4892C4.17949 11.7341 4.65811 12.8896 4.11809 13.7758C3.28212 15.1478 4.85218 16.7179 6.22417 15.8819C7.11045 15.3419 8.26594 15.8205 8.51078 16.8291C8.8898 18.3903 11.1102 18.3903 11.4892 16.8291C11.7341 15.8205 12.8896 15.3419 13.7758 15.8819C15.1478 16.7179 16.7179 15.1478 15.8819 13.7758C15.3419 12.8896 15.8205 11.7341 16.8291 11.4892C18.3903 11.1102 18.3903 8.8898 16.8291 8.51078C15.8205 8.26593 15.3419 7.11045 15.8819 6.22416C16.7179 4.85218 15.1478 3.28212 13.7758 4.11809C12.8896 4.65811 11.7341 4.17949 11.4892 3.17094ZM10 13C11.6569 13 13 11.6569 13 10C13 8.34315 11.6569 7 10 7C8.34315 7 7 8.34315 7 10C7 11.6569 8.34315 13 10 13Z"
                  fill="#111827"
              />
            </svg>
          </a-button>
          <a-button
              type="link"
              class="sidebar-toggler"
              @click="
              $emit('toggleSidebar', !sidebarCollapsed), resizeEventHandler()
            "
          >
            <svg
                width="20"
                height="20"
                xmlns="http://www.w3.org/2000/svg"
                viewBox="0 0 448 512"
            >
              <path
                  d="M16 132h416c8.837 0 16-7.163 16-16V76c0-8.837-7.163-16-16-16H16C7.163 60 0 67.163 0 76v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16z"
              />
            </svg>
          </a-button>

          <a-dropdown
              :trigger="['click']"
              overlayClassName="header-notifications-dropdown"
              :getPopupContainer="() => wrapper"
          >
            <a class="btn-sign-in">

              <div class="table-avatar-info">
                <a-avatar shape="circle" :size="24">{{ userInfo.name.slice(0, 1).toUpperCase() }}</a-avatar>
                <span>{{ userInfo.name }}</span>
              </div>

            </a>

            <a-list
                item-layout="horizontal"
                class="header-notifications-list"
                :data-source="notificationsData"
                slot="overlay"
            >
              <a-list-item slot="renderItem" slot-scope="item">
                <a-list-item-meta @click="item.event">
                  <span slot="title">{{ item.title }}</span>
                  <a-avatar
                      v-if="item.img"
                      slot="avatar"
                      shape="square"
                      :src="item.img"
                  />
                  <a-avatar
                      v-else
                      shape="square"
                      slot="avatar"
                      v-html="item.svg"
                  />
                </a-list-item-meta>
              </a-list-item>
            </a-list>
          </a-dropdown>

          <!-- / Header Control Buttons -->
        </a-col>
        <!-- / Header Control Column -->
      </a-row>
    </a-layout-header>
    <!--  /Layout Header -->
  </component>
  <!-- / Main Sidebar -->
</template>

<script>
import store from '@/store'
import {USER_INFO} from '@/store/mutation-types'

export default {
  props: {
    // Header fixed status.
    navbarFixed: {
      type: Boolean,
      default: false
    },

    // Sidebar collapsed status.
    sidebarCollapsed: {
      type: Boolean,
      default: false
    }
  },
  data() {
    return {
      // Fixed header/sidebar-footer ( Affix component ) top offset.
      top: 0,
      userInfo: {
        name: '',
        securityTokenKey: '',
        enabled: '',
        roles: [],
        email: ''
      },
      // Search input loading status.
      searchLoading: false,

      // The wrapper element to attach dropdowns to.
      wrapper: document.body,
      notificationsData: [
        {
          title: '密码修改',
          svg: `<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path class="fill-muted" d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z" fill="#111827"/>
                <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827"/>
                </svg>`,
          event: this.logout
        },
        {
          title: '退出登录',
          svg: `<svg v-else width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path fill-rule="evenodd" clip-rule="evenodd" d="M3 17C3 16.4477 3.44772 16 4 16H16C16.5523 16 17 16.4477 17 17C17 17.5523 16.5523 18 16 18H4C3.44772 18 3 17.5523 3 17ZM6.29289 6.70711C5.90237 6.31658 5.90237 5.68342 6.29289 5.29289L9.29289 2.29289C9.48043 2.10536 9.73478 2 10 2C10.2652 2 10.5196 2.10536 10.7071 2.29289L13.7071 5.29289C14.0976 5.68342 14.0976 6.31658 13.7071 6.70711C13.3166 7.09763 12.6834 7.09763 12.2929 6.70711L11 5.41421L11 13C11 13.5523 10.5523 14 10 14C9.44771 14 9 13.5523 9 13L9 5.41421L7.70711 6.70711C7.31658 7.09763 6.68342 7.09763 6.29289 6.70711Z" fill="#111827"/>
                </svg>`,
          event: this.logout
        }
      ]
    }
  },
  methods: {
    resizeEventHandler() {
      this.top = this.top ? 0 : -0.01
    },
    onSearch(value) {
    },
    logout() {
      store.dispatch('Logout')
    }
  },
  mounted: function () {
    // Set the wrapper to the proper element, layout wrapper.
    this.wrapper = document.getElementById('layout-dashboard')
  },
  created() {

    console.log(store.state)
    this.userInfo = store.state.user
    window.addEventListener('resize', this.resizeEventHandler)
  },
  destroyed() {
    // Removing window resize event listener.
    window.removeEventListener('resize', this.resizeEventHandler)
  }
}
</script>
<style type="scoped">
.ant-list-item-meta-content {
  margin-top: 10px;
}

.ant-list-item {
  cursor: pointer;
}

</style>
