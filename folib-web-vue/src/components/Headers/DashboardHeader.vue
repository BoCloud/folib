<template>
  <!-- Main Sidebar -->
  <component :is="navbarFixed ? 'a-affix' : 'div'" :offset-top="top" class="header">
    <!-- Layout Header -->
    <a-layout-header style="margin-bottom: 5px;margin-top: 10px;">
      <a-row type="flex">
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
        <a-col :span="8" :md="language === 'zh' ? 6 : 10" class="by-flex">
          <!-- Header Breadcrumbs -->
          <a-breadcrumb>
            <template v-for="(item, key) in realPath">
              <a-breadcrumb-item
                v-if="key == $route.meta.breadcrumbs.length - 1"
                :key="key"
                >{{ $t(item.name) }}
              </a-breadcrumb-item>
              <a-breadcrumb-item v-else :key="key">
                <router-link :to="item.path">{{ $t(item.name) }}</router-link>
              </a-breadcrumb-item>
            </template>
          </a-breadcrumb>
          <!-- / Header Breadcrumbs -->

          <!-- Header Page Title -->
          <!-- <div class="ant-page-header-heading">
            <span class="ant-page-header-heading-title">{{
              $t(this.$route.meta.title) || $t(this.$route.name)
            }}</span>
          </div>-->
          <!-- / Header Page Title -->
        </a-col>
        <!-- / Header Breadcrumbs & Title Column -->

        <!-- Header Breadcrumbs & Title Column -->
       
        <!-- / Header Breadcrumbs & Title Column -->

        <!-- Header Control Column -->
        <a-col :span="12" :md="language === 'zh' ? 17 : 13" class="header-control" v-if="userInfo.token">
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
                          <span v-if="!item.dictKey.includes('zip_')">{{ $t('Headers.Error') }}：</span>
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
          <!-- 多语言 -->
          <a-dropdown
              :trigger="['click']"
              overlayClassName="header-notifications-dropdown"
              :getPopupContainer="() => wrapper"
          >
            <a class="btn-sign-in">
              <div class="table-avatar-info">
                <svg t="1702547797250" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="1871" width="20" height="20"><path d="M162.816 672h199.68c-5.472-40.192-8.96-83.2-10.08-128H129.28c3.744 45.344 15.36 88.48 33.504 128z m37.248 64a384.32 384.32 0 0 0 223.712 149.824c-20.64-38.272-37.92-89.6-50.368-149.824H200.064z m661.12-64c18.144-39.52 29.76-82.656 33.504-128h-223.104a1166.624 1166.624 0 0 1-10.08 128h199.68z m-37.248 64H650.56c-12.448 60.192-29.728 111.552-50.368 149.824A384.32 384.32 0 0 0 823.904 736z m-399.232-64h174.592c4.544-39.52 7.424-82.656 8.384-128h-191.36c0.96 45.344 3.84 88.48 8.384 128z m9.312 64c17.44 96.896 45.856 160 77.984 160 32.128 0 60.544-63.104 77.984-160h-155.968z m-304.704-256h223.104c1.12-44.8 4.608-87.808 10.08-128h-199.68a381.632 381.632 0 0 0-33.504 128z m765.376 0a381.632 381.632 0 0 0-33.504-128h-199.68c5.472 40.192 8.96 83.2 10.08 128h223.104zM416.32 480h191.36a1379.936 1379.936 0 0 0-8.384-128h-174.592a1379.936 1379.936 0 0 0-8.384 128zM200.064 288h173.344c12.448-60.192 29.728-111.552 50.368-149.824A384.32 384.32 0 0 0 200.096 288z m623.872 0a384.32 384.32 0 0 0-223.712-149.824c20.64 38.272 37.92 89.6 50.368 149.824h173.344zM434.016 288h155.968C572.544 191.104 544.128 128 512 128c-32.128 0-60.544 63.104-77.984 160zM512 960C264.576 960 64 759.424 64 512S264.576 64 512 64s448 200.576 448 448-200.576 448-448 448z" fill="#333333" p-id="1872"></path></svg>
              </div>
            </a>
            <a-list
                item-layout="horizontal"
                class="header-notifications-list"
                :data-source="LanguageList"
                slot="overlay"
            >
              <a-list-item slot="renderItem" slot-scope="item" class="user-title">
                <a-list-item-meta @click="changeLanguage(item.value)">
                  <span slot="title">{{ item.title }}</span>
                  <a-avatar
                      shape="square"
                      slot="avatar"
                      v-html="item.svg"
                  />
                </a-list-item-meta>
              </a-list-item>
            </a-list>
          </a-dropdown>
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
                  <span slot="title">{{ $t(item.i18nKey) }}</span>
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
        <a-col :span="12" :md="language === 'zh' ? 17 : 13" class="header-control" v-else>
          <!-- Header Control Buttons -->
          <!-- 多语言 -->
          <a-dropdown
              :trigger="['click']"
              overlayClassName="header-notifications-dropdown"
              :getPopupContainer="() => wrapper"
          >
            <a class="btn-sign-in">
              <div class="table-avatar-info">
                <svg t="1702547797250" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="1871" width="20" height="20"><path d="M162.816 672h199.68c-5.472-40.192-8.96-83.2-10.08-128H129.28c3.744 45.344 15.36 88.48 33.504 128z m37.248 64a384.32 384.32 0 0 0 223.712 149.824c-20.64-38.272-37.92-89.6-50.368-149.824H200.064z m661.12-64c18.144-39.52 29.76-82.656 33.504-128h-223.104a1166.624 1166.624 0 0 1-10.08 128h199.68z m-37.248 64H650.56c-12.448 60.192-29.728 111.552-50.368 149.824A384.32 384.32 0 0 0 823.904 736z m-399.232-64h174.592c4.544-39.52 7.424-82.656 8.384-128h-191.36c0.96 45.344 3.84 88.48 8.384 128z m9.312 64c17.44 96.896 45.856 160 77.984 160 32.128 0 60.544-63.104 77.984-160h-155.968z m-304.704-256h223.104c1.12-44.8 4.608-87.808 10.08-128h-199.68a381.632 381.632 0 0 0-33.504 128z m765.376 0a381.632 381.632 0 0 0-33.504-128h-199.68c5.472 40.192 8.96 83.2 10.08 128h223.104zM416.32 480h191.36a1379.936 1379.936 0 0 0-8.384-128h-174.592a1379.936 1379.936 0 0 0-8.384 128zM200.064 288h173.344c12.448-60.192 29.728-111.552 50.368-149.824A384.32 384.32 0 0 0 200.096 288z m623.872 0a384.32 384.32 0 0 0-223.712-149.824c20.64 38.272 37.92 89.6 50.368 149.824h173.344zM434.016 288h155.968C572.544 191.104 544.128 128 512 128c-32.128 0-60.544 63.104-77.984 160zM512 960C264.576 960 64 759.424 64 512S264.576 64 512 64s448 200.576 448 448-200.576 448-448 448z" fill="#333333" p-id="1872"></path></svg>
              </div>
            </a>
            <a-list
                item-layout="horizontal"
                class="header-notifications-list"
                :data-source="LanguageList"
                slot="overlay"
            >
              <a-list-item slot="renderItem" slot-scope="item" class="user-title">
                <a-list-item-meta @click="changeLanguage(item.value)">
                  <span slot="title">{{ item.title }}</span>
                  <a-avatar
                      shape="square"
                      slot="avatar"
                      v-html="item.svg"
                  />
                </a-list-item-meta>
              </a-list-item>
            </a-list>
          </a-dropdown>
          <a-button
            type="link"
            ref="secondarySidebarTriggerBtn"
            @click="toLogin()"
          >
            <a-icon type="login" :style="{fontSize: '20px'}"/>
            <span class="ml-5 login-span">{{ $t('Headers.Login') }}</span>
          </a-button>
          <!-- / Header Control Buttons -->
        </a-col>
        <!-- / Header Control Column -->
      </a-row>
    </a-layout-header>
    <!--  /Layout Header -->
    <!-- <a-tooltip placement="topLeft">
        <template slot="title">
            <span>{{ $t('Storage.ModifyStorageSpace') }}</span>
        </template> -->
    <!-- <div class="switch_mode" v-if="this.$route.name === 'storagesHome'">
      <div @click="checkMode(false)" class="img-sty" :class="isChecked ? '' : 'isActive'">
        <img src="./images/list.svg" width="20" alt="">
      </div>
      <div @click="checkMode(true)" class="img-sty" :class="isChecked ? 'isActive' : ''">
        <img src="./images/tree.svg" width="20" alt="">
      </div>
      <div :style="switchDisabled?'display:block;':'display:none;'" class="disabled_sty"></div>
    </div> -->
    <!-- </a-tooltip> -->
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
import {setLanguage} from "@/locale"

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
          title: "个人信息",
          i18nKey: 'Headers.PersonalInformation',
          svg: `<svg t="1703588933319" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="21112" width="20" height="20">
                <path d="M903.111111 938.296889a391.651556 391.651556 0 0 0-233.102222-346.510222 296.448 296.448 0 0 0 139.264-251.875556C809.244444 175.644444 676.266667 42.666667 512 42.666667S214.755556 175.644444 214.755556 339.911111c0 106.382222 55.523556 199.480889 140.003555 251.875556-134.513778 59.448889-229.176889 191.630222-233.870222 346.510222v3.925333c0 21.902222 17.208889 39.111111 39.111111 39.111111S199.111111 964.124444 199.111111 942.222222c3.896889-168.96 142.364444-305.066667 312.888889-305.066666s308.963556 136.106667 312.888889 305.066666c0 21.902222 17.208889 39.111111 39.111111 39.111111s39.111111-17.208889 39.111111-39.111111v-3.925333zM512 558.933333a218.652444 218.652444 0 0 1-219.022222-219.022222c0-121.258667 97.792-219.022222 219.022222-219.022222s218.993778 97.792 218.993778 219.022222c0 121.230222-97.735111 219.022222-218.993778 219.022222z" fill="" p-id="21113"/>
                </svg>`,
          event: this.toPersonl,
        },{
          title: "退出登录",
          i18nKey: 'Headers.LogOut',
          svg: `<svg v-else width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path fill-rule="evenodd" clip-rule="evenodd" d="M3 17C3 16.4477 3.44772 16 4 16H16C16.5523 16 17 16.4477 17 17C17 17.5523 16.5523 18 16 18H4C3.44772 18 3 17.5523 3 17ZM6.29289 6.70711C5.90237 6.31658 5.90237 5.68342 6.29289 5.29289L9.29289 2.29289C9.48043 2.10536 9.73478 2 10 2C10.2652 2 10.5196 2.10536 10.7071 2.29289L13.7071 5.29289C14.0976 5.68342 14.0976 6.31658 13.7071 6.70711C13.3166 7.09763 12.6834 7.09763 12.2929 6.70711L11 5.41421L11 13C11 13.5523 10.5523 14 10 14C9.44771 14 9 13.5523 9 13L9 5.41421L7.70711 6.70711C7.31658 7.09763 6.68342 7.09763 6.29289 6.70711Z" fill="#111827"/>
                </svg>`,
          event: this.logout,
        },
      ],
      LanguageList: [
        {
          title: "中文",
          value: 'zh',
          svg: `<svg t="1703595059282" class="icon" viewBox="0 0 1501 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="22719" width="36px" height="36px"><path d="M0.205 1003.52h1500.023V0.683H0.205z" fill="#DE2910" p-id="22720"></path><path d="M516.3 413.286l-2.184 33.724 31.403 12.425-32.768 8.465-2.185 33.723-18.022-28.672-32.905 8.397 21.573-25.941-18.023-28.672 31.54 12.493 21.572-25.942z m80.897-103.15l10.24 32.153h33.928l-27.306 19.934 10.24 32.154-27.307-20.07-27.307 19.933 10.513-32.085-27.306-19.798h33.86l10.445-32.222z m34.543-137.08l-15.292 30.174 23.893 23.893-33.382-5.325-15.497 30.174-5.188-33.587-33.45-5.188 30.242-15.36-5.257-33.383 23.757 23.894 30.174-15.292zM516.437 63.693l-2.048 33.655 31.403 12.493-32.836 8.465-2.185 33.656-18.09-28.672-32.7 8.465 21.572-25.942-18.022-28.672 31.402 12.493 21.504-25.941z m-267.605 51.063l31.676 97.622h102.81l-83.081 60.28 31.676 97.825-83.15-60.348-83.08 60.348 31.676-97.826-83.08-60.348h102.809l31.676-97.621z" fill="#FFDE00" p-id="22721"></path></svg>`,
          img: '@/assets/img/icon-CNH.png'
        },{
          title: "English",
          value: 'en',
          svg: `<svg t="1703668600066" class="icon" viewBox="0 0 1536 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="31283" width="36px" height="36px"><path d="M0 0h1536v1024H0V0z" fill="#00458F" p-id="31284"></path><path d="M1536 358.4H921.6V0h-307.2v358.4H0v307.2h614.4V1024h307.2V665.6H1536v-307.2z" fill="#FFFFFF" p-id="31285"></path><path d="M1536 905.6L176 0H0v118.4L1360 1024H1536v-118.4z" fill="#FFFFFF" p-id="31286"></path><path d="M0 905.6L1360 0H1536v118.4L176 1024H0v-118.4z" fill="#FFFFFF" p-id="31287"></path><path d="M1536 422.4H857.6V0h-179.2v422.4H0v179.2h678.4V1024h179.2V601.6H1536v-179.2z" fill="#CC3440" p-id="31288"></path><path d="M1536 1020.8v-80l-412.8-275.2h-121.6L1536 1020.8zM1536 0h-121.6L921.6 326.4v28.8h76.8L1536 0zM534.4 665.6L0 1020.8v3.2h115.2l496-329.6v-28.8h-76.8zM0 0v80l416 278.4h121.6L0 0z" fill="#CC3440" p-id="31289"></path></svg>`,
          img: '@/assets/img/icon-CNH.png'
        }
      ],
      uploadProcessList: [],
      notFinishUploadList: [],
      visible: false,
      delSvg: `<svg t="1678377092023" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="4541" width="36" height="36"><path d="M0 0m136.533333 0l750.933334 0q136.533333 0 136.533333 136.533333l0 750.933334q0 136.533333-136.533333 136.533333l-750.933334 0q-136.533333 0-136.533333-136.533333l0-750.933334q0-136.533333 136.533333-136.533333Z" fill="#d81e06" opacity=".08" p-id="4542"></path><path d="M592.145067 690.8928l22.186666-276.1728H391.509333l22.186667 276.1728c0.273067 3.3792 2.833067 5.905067 5.973333 5.905067h166.468267c3.140267 0 5.7344-2.525867 6.007467-5.905067z m-130.286934-322.218667c0 1.092267-0.4096 2.048-0.6144 3.140267h83.319467c-0.170667-1.058133-0.580267-2.048-0.580267-3.140267 0-24.337067-18.432-44.1344-41.0624-44.1344s-41.0624 19.797333-41.0624 44.1344z m216.814934 3.140267c11.025067 0 19.968 9.591467 19.968 21.435733 0 11.8784-8.942933 21.469867-19.968 21.469867h-24.2688l-22.493867 279.893333c-2.048 25.7024-21.742933 45.090133-45.7728 45.090134h-166.468267c-23.995733 0-43.690667-19.387733-45.738666-45.124267l-22.528-279.893333h-24.234667c-11.025067 0-19.968-9.557333-19.968-21.435734 0-11.844267 8.942933-21.435733 19.968-21.435733H427.861333c-0.2048-1.058133-0.6144-2.048-0.6144-3.140267 0-44.8512 33.928533-81.3056 75.6736-81.3056 41.710933 0 75.6736 36.4544 75.6736 81.3056 0 1.092267-0.443733 2.048-0.6144 3.140267H678.673067z m-206.9504 276.343467c-9.557333 0-17.3056-8.328533-17.3056-18.602667v-127.5904c0-10.24 7.748267-18.602667 17.3056-18.602667s17.3056 8.362667 17.3056 18.602667v127.5904c0 10.24-7.748267 18.602667-17.3056 18.602667z m65.4336 3.242666c-9.557333 0-17.3056-8.328533-17.3056-18.602666V498.688c0-10.24 7.748267-18.602667 17.3056-18.602667s17.3056 8.328533 17.3056 18.602667v134.075733c0 10.24-7.748267 18.602667-17.3056 18.602667z" fill="#d81e06" p-id="4543"></path></svg>`,
      clearSvg: `<svg t="1678379444252" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="6076" width="16" height="16"><path d="M433.664 250.88L773.12 590.336 599.466667 837.162667a42.666667 42.666667 0 0 1-65.066667 5.632l-61.333333-61.333334v-130.88h-130.858667L181.205333 489.6a42.666667 42.666667 0 0 1 5.632-65.066667l246.826667-173.632z m38.378667-26.986667l66.133333-46.528a42.666667 42.666667 0 0 1 54.72 4.714667l89.130667 89.152 93.781333-93.781333a21.333333 21.333333 0 0 1 30.165333 0l35.2 35.2a21.333333 21.333333 0 0 1 0 30.186666l-93.76 93.76 94.506667 94.506667a42.666667 42.666667 0 0 1 4.714667 54.72l-46.506667 66.133333-328.106667-328.064z" fill="#2A2A37" p-id="6077"></path></svg>`,
      isChecked: false,
    };
  },
  methods: {
    // // 制品仓库页面专属。用于切换仓库和存储空间的展示模式
    // checkMode(key){
    //   if(this.switchDisabled){
    //     return
    //   }
    //   setTimeout(() => {
    //     this.isChecked = key
    //     this.$store.commit('setIsChecked',key)
    //   }, 0);
    // },
    changeLanguage(val) {
      setLanguage(val)
      this.$forceUpdate()
    },
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
      }, 3000);
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
    this.$store.commit('setIsChecked',false)
    this.isChecked = false
  },
  computed: {
    switchDisabled(){
      return this.$store.state.switchDisabled
    },
    language() {
      return this.$store.state.language.lang
    },
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
    },
    switchDisabled(val){
      console.log(val,'switchDisabled')
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
  position: relative;
  display: inline-block;
  width: 24px;
  height: 24px;
}

.login-span {
  color: #8C8DA5;
  font-size: 14px;
  vertical-align: middle;
}

.switch_mode{
  display: flex;
  justify-content: space-between;
  align-items: center;
  width: 80px;
  background: rgba(255,255,255,0.5);
  padding:5px;
  border-radius: 4px;
  position: absolute;
  top: 45px;
  left: 250px;
  
  .img-sty{
    opacity: 1 !important;
    cursor: pointer;
    background: rgba(255,255,255,0.5);
    border-radius: 4px;
    box-shadow: 0px 1px 6px 1px rgba(0,0,0,0.1);
    padding: 4px 5px;
    transition: all 0.3s;
  }

  .isActive{
    background: #396ed1;
    box-shadow: 0px 1px 6px 1px rgba(0,0,0,0.2);
  }
  .disabled_sty{
    width: 80px;
    height: 40px;
    position: absolute;
    cursor: not-allowed;
    z-index: 9999;
  }
}
</style>
