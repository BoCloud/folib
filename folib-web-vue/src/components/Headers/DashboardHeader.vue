<template>
  <!-- Main Sidebar -->
  <component :is="navbarFixed ? 'a-affix' : 'div'" :offset-top="top" class="header">
    <!-- Layout Header -->
    <a-layout-header>
      <a-row type="flex">
        <!-- Header Breadcrumbs & Title Column -->
        <a-col :span="20" :md="6">
          <!-- Header Breadcrumbs -->
          <a-breadcrumb>
            <template v-for="(item, key) in realPath">
              <a-breadcrumb-item
                v-if="key == $route.meta.breadcrumbs.length - 1"
                :key="key"
                >{{ item.name }}
              </a-breadcrumb-item>
              <a-breadcrumb-item v-else :key="key">
                <router-link :to="item.path">{{ item.name }}</router-link>
              </a-breadcrumb-item>
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
        <a-col :span="24" :md="17" class="header-control" v-if="userInfo.token">
          <!-- Header Control Buttons -->
					<a-dropdown :trigger="['click']" v-model="visible" overlayClassName="header-notifications-dropdown" :getPopupContainer="() => wrapper">
						<a-badge :count="uploadProcessList?uploadProcessList.length:0">
							<a class="ant-dropdown-link" @click="e => e.preventDefault()">
								<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
									<path d="M10 2C6.68632 2 4.00003 4.68629 4.00003 8V11.5858L3.29292 12.2929C3.00692 12.5789 2.92137 13.009 3.07615 13.3827C3.23093 13.7564 3.59557 14 4.00003 14H16C16.4045 14 16.7691 13.7564 16.9239 13.3827C17.0787 13.009 16.9931 12.5789 16.7071 12.2929L16 11.5858V8C16 4.68629 13.3137 2 10 2Z" fill="#111827"/>
									<path d="M10 18C8.34315 18 7 16.6569 7 15H13C13 16.6569 11.6569 18 10 18Z" fill="#111827"/>
								</svg>
							</a>
						</a-badge>
						<a-list item-layout="horizontal" class="header-notifications-list" :data-source="uploadProcessList" slot="overlay">
              <div v-if="uploadProcessList && uploadProcessList.length > 0" slot="header" class="upload-process-header">
                <span @click="uploadProcessRemove('')">
                  <svg t="1678379444252" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="6076" width="20" height="20"><path d="M433.664 250.88L773.12 590.336 599.466667 837.162667a42.666667 42.666667 0 0 1-65.066667 5.632l-61.333333-61.333334v-130.88h-130.858667L181.205333 489.6a42.666667 42.666667 0 0 1 5.632-65.066667l246.826667-173.632z m38.378667-26.986667l66.133333-46.528a42.666667 42.666667 0 0 1 54.72 4.714667l89.130667 89.152 93.781333-93.781333a21.333333 21.333333 0 0 1 30.165333 0l35.2 35.2a21.333333 21.333333 0 0 1 0 30.186666l-93.76 93.76 94.506667 94.506667a42.666667 42.666667 0 0 1 4.714667 54.72l-46.506667 66.133333-328.106667-328.064z" fill="#2A2A37" p-id="6077"></path></svg>
                </span>
              </div>
							<a-list-item slot="renderItem" slot-scope="item">
								<a-list-item-meta>
									<template #description>
										<span>
                      <a-progress class="upload-process" :percent="item.dictValue" :showInfo="true" :status="(item.comment && item.comment.length > 0 && !item.dictKey.includes('zip_'))?'exception':item.dictValue<100?'active':'success'" />
                    </span>
									</template>
									<a slot="title" href="#">
                    <a-tooltip placement="top">
                      <template slot="title">
                        <span v-if="item.comment && item.comment.length >0">
                          <span v-if="!item.dictKey.includes('zip_')">错误：</span>
                          {{item.comment}}
                        </span>
                      </template>
                      {{ item.alias }}
                    </a-tooltip>
                  </a>
									<a-avatar
                    @click="uploadProcessRemove(item.dictKey)"
										shape="square"
										slot="avatar"  v-html="delSvg"/>
								</a-list-item-meta>
							</a-list-item>
						</a-list>
					</a-dropdown>
          <a-button
            type="link"
            ref="secondarySidebarTriggerBtn"
            @click="toPersonl()"
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
                <svg class="icon folib-avatar" :style="{ fontSize: '32px' }" aria-hidden="true" v-if="userInfo.avatar">
                  <use :xlink:href="'#'+ userInfo.avatar"></use>
                </svg>
                <a-avatar shape="circle" :size="24" v-else>{{
                  userInfo.name.slice(0, 1).toUpperCase()
                }}</a-avatar>
                <span>{{ userInfo.name }}</span>
              </div>
            </a>

            <a-list
              item-layout="horizontal"
              class="header-notifications-list"
              :data-source="notificationsData"
              slot="overlay"
            >
              <a-list-item class="user-title" slot="renderItem" slot-scope="item">
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
        <a-col :span="24" :md="17" class="header-control" v-else>
          <!-- Header Control Buttons -->
          <a-button
            type="link"
            ref="secondarySidebarTriggerBtn"
            @click="toLogin()"
          >
            <a-icon type="login" :style="{fontSize: '20px'}"/>
            <span class="ml-5 login-span">登录</span>
          </a-button>
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
import store from "@/store";
import {USER_INFO} from '@/store/mutation-types'
import routers from "../../router";
import {
  queryArtifactUploadProcess,
  deleteArtifactUploadProcess,
} from "@/api/artifact"
import { hasRole, isAdmin, isAnonymous, isLogin } from "@/utils/permission"

export default {
  props: {
    // Header fixed status.
    navbarFixed: {
      type: Boolean,
      default: false,
    },

    // Sidebar collapsed status.
    sidebarCollapsed: {
      type: Boolean,
      default: false,
    },
  },
  
  data() { 
    return {
      // Fixed header/sidebar-footer ( Affix component ) top offset.
      top: 0,
      userInfo: {
        name: "",
        securityTokenKey: "",
        enabled: "",
        roles: [],
        email: "",
        avatar: "",
      },
      // Search input loading status.
      searchLoading: false,

      // The wrapper element to attach dropdowns to.
      wrapper: document.body,
      notificationsData: [
        // {
        //   title: '密码修改',
        //   svg: `<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
        //         <path class="fill-muted" d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z" fill="#111827"/>
        //         <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827"/>
        //         </svg>`,
        //   event: this.logout
        // },
        {
          title: "退出登录",
          svg: `<svg v-else width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path fill-rule="evenodd" clip-rule="evenodd" d="M3 17C3 16.4477 3.44772 16 4 16H16C16.5523 16 17 16.4477 17 17C17 17.5523 16.5523 18 16 18H4C3.44772 18 3 17.5523 3 17ZM6.29289 6.70711C5.90237 6.31658 5.90237 5.68342 6.29289 5.29289L9.29289 2.29289C9.48043 2.10536 9.73478 2 10 2C10.2652 2 10.5196 2.10536 10.7071 2.29289L13.7071 5.29289C14.0976 5.68342 14.0976 6.31658 13.7071 6.70711C13.3166 7.09763 12.6834 7.09763 12.2929 6.70711L11 5.41421L11 13C11 13.5523 10.5523 14 10 14C9.44771 14 9 13.5523 9 13L9 5.41421L7.70711 6.70711C7.31658 7.09763 6.68342 7.09763 6.29289 6.70711Z" fill="#111827"/>
                </svg>`,
          event: this.logout,
        },
      ],
      uploadProcessList: [],
      notFinishUploadList: [],
      visible: false,
      delSvg: `<svg t="1678377092023" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="4541" width="36" height="36"><path d="M0 0m136.533333 0l750.933334 0q136.533333 0 136.533333 136.533333l0 750.933334q0 136.533333-136.533333 136.533333l-750.933334 0q-136.533333 0-136.533333-136.533333l0-750.933334q0-136.533333 136.533333-136.533333Z" fill="#d81e06" opacity=".08" p-id="4542"></path><path d="M592.145067 690.8928l22.186666-276.1728H391.509333l22.186667 276.1728c0.273067 3.3792 2.833067 5.905067 5.973333 5.905067h166.468267c3.140267 0 5.7344-2.525867 6.007467-5.905067z m-130.286934-322.218667c0 1.092267-0.4096 2.048-0.6144 3.140267h83.319467c-0.170667-1.058133-0.580267-2.048-0.580267-3.140267 0-24.337067-18.432-44.1344-41.0624-44.1344s-41.0624 19.797333-41.0624 44.1344z m216.814934 3.140267c11.025067 0 19.968 9.591467 19.968 21.435733 0 11.8784-8.942933 21.469867-19.968 21.469867h-24.2688l-22.493867 279.893333c-2.048 25.7024-21.742933 45.090133-45.7728 45.090134h-166.468267c-23.995733 0-43.690667-19.387733-45.738666-45.124267l-22.528-279.893333h-24.234667c-11.025067 0-19.968-9.557333-19.968-21.435734 0-11.844267 8.942933-21.435733 19.968-21.435733H427.861333c-0.2048-1.058133-0.6144-2.048-0.6144-3.140267 0-44.8512 33.928533-81.3056 75.6736-81.3056 41.710933 0 75.6736 36.4544 75.6736 81.3056 0 1.092267-0.443733 2.048-0.6144 3.140267H678.673067z m-206.9504 276.343467c-9.557333 0-17.3056-8.328533-17.3056-18.602667v-127.5904c0-10.24 7.748267-18.602667 17.3056-18.602667s17.3056 8.362667 17.3056 18.602667v127.5904c0 10.24-7.748267 18.602667-17.3056 18.602667z m65.4336 3.242666c-9.557333 0-17.3056-8.328533-17.3056-18.602666V498.688c0-10.24 7.748267-18.602667 17.3056-18.602667s17.3056 8.328533 17.3056 18.602667v134.075733c0 10.24-7.748267 18.602667-17.3056 18.602667z" fill="#d81e06" p-id="4543"></path></svg>`,
      clearSvg: `<svg t="1678379444252" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="6076" width="16" height="16"><path d="M433.664 250.88L773.12 590.336 599.466667 837.162667a42.666667 42.666667 0 0 1-65.066667 5.632l-61.333333-61.333334v-130.88h-130.858667L181.205333 489.6a42.666667 42.666667 0 0 1 5.632-65.066667l246.826667-173.632z m38.378667-26.986667l66.133333-46.528a42.666667 42.666667 0 0 1 54.72 4.714667l89.130667 89.152 93.781333-93.781333a21.333333 21.333333 0 0 1 30.165333 0l35.2 35.2a21.333333 21.333333 0 0 1 0 30.186666l-93.76 93.76 94.506667 94.506667a42.666667 42.666667 0 0 1 4.714667 54.72l-46.506667 66.133333-328.106667-328.064z" fill="#2A2A37" p-id="6077"></path></svg>`
    };
  },
  methods: {
    resizeEventHandler() {
      this.top = this.top ? 0 : -0.01;
    },
    onSearch(value) {},
    logout() {
      store.dispatch("Logout");
    },
    interval() {
      const intervalId = setInterval(() => {
        if (this.incompleted()) {
          this.notFinishUploadList.forEach(element => {
            this.getProgressRate(element)
          })
        } else {
          clearInterval(intervalId)
        }
      }, 300);
    },
    incompleted (){
      this.notFinishUploadList = this.uploadProcessList.filter(item => item.dictValue < 100 && (!item.comment || item.comment.length <1))
      return this.notFinishUploadList.length > 0
    },
    //获取进度
    getProgressRate(element) {
      queryArtifactUploadProcess(element.dictKey).then((res) => {
        if (res && res.length > 0) {
          let data = res[0];
          element.dictValue = new Number(data.dictValue)
          element.comment = data.comment
        }
      })
    },
    queryAllProcess () {
      queryArtifactUploadProcess('').then((res) => {
        this.uploadProcessList = res
        if (this.uploadProcessList) {
          this.uploadProcessList.forEach(item => {
            item.dictValue = new Number(item.dictValue)
          })
          this.interval()
        }
      })
    },
    uploadProcessRemove(uuid) {
      deleteArtifactUploadProcess(uuid).then(() => {
        this.queryAllProcess()
      })
    },
    toPersonl(){
      this.$router.push('/personal')
    },
    toLogin() {
      this.$router.push({ name: 'login' })
    }
  },
  mounted: function () {
    // Set the wrapper to the proper element, layout wrapper.
    this.wrapper = document.getElementById("layout-dashboard");
  },
  computed: {
    realPath() {
      const breadcrumbs = this.$route.meta.breadcrumbs;
      const list = [];
      const routes = this.$router.options.routes;
      breadcrumbs.forEach((p, i) => {
        
        const item = routes.find((o) => o.meta.title === p);
        list.push({ name: p, path: item ? item.path : "" });
      });

      return list;
    },
  },
  created() {
    // console.log(store.state)
    this.userInfo = store.state.user;
    window.addEventListener("resize", this.resizeEventHandler);
    if (isAnonymous()) {
      this.$emit('minimizeSidebar')
      this.resizeEventHandler()
    }
  },
  destroyed() {
    // Removing window resize event listener.
    window.removeEventListener("resize", this.resizeEventHandler);
  },
  watch: {
    visible: {
      handler(val) {
        if (isLogin()) {
          this.queryAllProcess()
        }
      },
      immediate:true
    }  
  },
};
</script>
<style lang="scss" scoped>
.user-title::v-deep .ant-list-item-meta-content {
  margin-top: 10px;
}

.ant-list-item,.upload-process-header span {
  cursor: pointer;
}

.upload-process-header {
  text-align: -webkit-right;
}

.upload-process::v-deep .ant-progress-text {
  vertical-align: text-top !important;
}

.header .folib-avatar {
  display: inline-block;
  width: 24px;
  height: 24px;
}

.login-span {
  color: white;
  font-size: 14px;
  vertical-align: middle;
}
</style>
