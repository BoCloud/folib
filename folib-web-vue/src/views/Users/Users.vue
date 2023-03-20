<!-- 
	This is the Billing page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>
  <div>

    <a-row type="flex" :gutter="24">

      <!-- Billing Info Column -->
      <a-col :span="24" :md="currentUser ? 14 : 24">
        <a-row type="flex" :gutter="24">
          <a-col :span="24" class="mb-24">
            <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ paddingTop: 0, }">
              <template #title>
                <a-row type="flex" align="middle">
                  <a-col :span="24" :md="12">
                    <h6 class="font-semibold m-0">用户管理</h6>
                  </a-col>
                  <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                    <a-button type="primary" @click="userCreate()">
                      添加 用户
                    </a-button>
                  </a-col>
                </a-row>
              </template>
              <a-row :gutter="[24, 24]">
                <a-col :span="24" :md="8">
                  <a-card class="payment-method-card">
                    <img src="images/folib/userAdmin.svg" alt="">
                    <h6 class="card-number">用户总数</h6>
                    <a-button type="link">
                      <h6 class="card-number">{{ allUsers.length }}</h6>
                    </a-button>
                  </a-card>
                </a-col>
                <a-col :span="24" :md="8">
                  <a-card class="payment-method-card">
                    <img src="images/folib/userAdmin.svg" alt="">
                    <h6 class="card-number">管理员数</h6>
                    <a-button type="link">
                      <h6 class="card-number">{{ getUserCount("ADMIN").length }}</h6>
                    </a-button>
                  </a-card>
                </a-col>
                <a-col :span="24" :md="8">
                  <a-card class="payment-method-card">
                    <img src="images/folib/userAdmin.svg" alt="">
                    <h6 class="card-number">匿名用户数</h6>
                    <a-button type="link">
                      <h6 class="card-number">{{ getUserCount("ANONYMOUS").length }}</h6>
                    </a-button>
                  </a-card>
                </a-col>

              </a-row>
            </a-card>
          </a-col>
          <a-col :span="24" class="mb-24">
            <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }">
              <template #title>
                <h6 class="font-semibold m-0">用户列表</h6>
              </template>
              <a-row :gutter="[24, 24]">
                <a-col :span="24" class="ml-20">
                  <a-row :gutter="[24, 24]">
                    <a-col :span="3" class="">
                      <a-input-search v-model="userQuery.username" placeholder="输入用户名查询" @search="searchUser()"/>
                    </a-col>
                    <a-col :span="3" class="ml-10">
                      <a-input-search v-model="userQuery.email" placeholder="输入邮箱查询" @search="searchUser()"/>
                    </a-col>
                  </a-row>
                </a-col>
                <a-col :span="24">
                  <a-list class="mb-10 mr-10 ml-10" item-layout="vertical" size="large" :data-source="users" :loading="userLoading"
                    :pagination="userTotal === 0 ? false : { pageSize: userPage.limit, total: userTotal, showLessItems: false, onChange: pageChange, current: userPage.page  }">
                    <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
                      <label>
                        <a-col :span="24">
                          <a-card :bordered="false" class="card-billing-info" @click.prevent="getUserDetial(item.username)">
                            <div class="col-info">
                              <a-descriptions :title="item.username" :column="1">
                                <a-descriptions-item label="是否可用">
                                  {{ item.enabled ? "可用" : "不可用" }}
                                </a-descriptions-item>
                                <a-descriptions-item label="Email">
                                  {{ item.email ? item.email : "" }}
                                </a-descriptions-item>
                                <a-descriptions-item label="角色信息">
                                  {{ item.roles }}
                                </a-descriptions-item>
                              </a-descriptions>
                            </div>
                            <div class="col-action">
                              <a-button type="link" size="small" @click.stop="delUserHandle(item.username)">
                                <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                                  <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                                    d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                    fill="#111827" />
                                </svg>
                                <span class="text-danger">DELETE</span>
                              </a-button>
                              <a-button type="link" size="small" @click="userEditHandle">
                                <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                                  <path class="fill-muted"
                                    d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                                    fill="#111827" />
                                  <path class="fill-muted"
                                    d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827" />
                                </svg>
                                <span class="text-dark">EDIT</span>
                              </a-button>
                            </div>
                          </a-card>
                        </a-col>
                      </label>
                      <template #extra>
                      </template>
                    </a-list-item>
                  </a-list>
                </a-col>
              </a-row>
            </a-card>

          </a-col>
        </a-row>
      </a-col>
      <a-col :span="24" v-if="currentUser" :md="10" class="mb-24">
        <a-row type="flex" :gutter="24">
          <a-col :span="24" class="mb-24">
            <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
              v-if="currentUser">
              <template #title>
                <h6 class="font-semibold m-0">{{ userNotEdit ? "用户信息" : "用户编辑" }}</h6>
              </template>
              <template slot="extra" class="mb-0" v-if="!userNotEdit">
                <div class="col-action">
                  <a-button type="link" size="small" @click="userEditCancelHandle">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                        fill="#111827" />
                    </svg>
                    <span class="text-danger">取消</span>
                  </a-button>
                  <a-button type="link" size="small" @click="userEditSaveHandle">
                    <a-icon type="save" theme="twoTone" />
                    <span class="text-dark">保存</span>
                  </a-button>
                </div>
              </template>
              <a-form-model ref="userForm" :model="currentUser.user" :rules="rules" :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" label="用户名" :colon="false" prop="username" :required="true">
                      <a-input :disabled="userNotEdit" placeholder="请输入用户名" v-model="currentUser.user.username" />
                    </a-form-model-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" label="密码" :colon="false" prop="password"
                      :required="passwordRequired">
                      <a-input-password :disabled="userNotEdit" autocomplete="new-password" placeholder="******"
                        v-model="currentUser.user.password" />
                    </a-form-model-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" label="EMAIL" :colon="false">
                      <a-input :disabled="userNotEdit" v-model="currentUser.user.email" placeholder="请输入邮箱，非必填" />
                    </a-form-model-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" label="是否激活" :colon="false">
                      <span class="mr-15">{{ true ? '开启' : '关闭' }}</span>
                      <a-switch :disabled="userNotEdit" default-checked v-model="currentUser.user.enabled" />
                    </a-form-model-item>
                  </a-col>
                </a-row>
              </a-form-model>
            </a-card>
          </a-col>
          <a-col :span="24" class="mb-24">
            <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
              v-if="currentUser">
              <template #title>
                <h6 class="font-semibold m-0">{{ userNotEdit ? "角色信息" : "角色编辑" }}</h6>
              </template>
              <div v-for="(item, index) in currentUser.assignableRoles" :key="index">
                <hr class="gradient-line">
                <a-row type="flex" align="middle">
                  <a-col>
                    <a-avatar :size="48" :src="'images/folib/' + roleLogBuild(item.name) + '.svg'" />
                  </a-col>
                  <a-col class="pl-15">
                    <h6 class="mb-0">{{ item.name }}</h6>
                    <a class="text-dark">{{ item.description }}</a>
                  </a-col>
                  <a-col :span="24" :md="8" class="ml-auto"
                    style="display: flex; align-items: center; justify-content: flex-end">
                    <span class="mr-15">{{ item.enabled ? '开启' : '关闭' }}</span>
                    <a-switch :disabled="userNotEdit" default-checked v-model="item.enabled" />
                  </a-col>
                </a-row>
              </div>
            </a-card>
          </a-col>
        </a-row>
      </a-col>
    </a-row>
    <a-modal v-model="deleteVisible" title="删除确认" :footer="null" :forceRender="true" on-back="deleteVisible = false">

      <a-row :gutter="[24]">
        <a-col :span="24">
          <h6 v-if="deleteVisible" class="text-center font-regular">你确定要删除<a>{{ willDelUserName }}</a>这个用户么？</h6>
        </a-col>
        <a-col :span="24" class="text-right">
          <a-button key="submit" class="px-30" size="small" type="danger" @click="delUser(willDelUserName)">删除
          </a-button>
          <a-button key="back" @click="deleteVisible = false" class="px-30 ml-10" size="small">取消</a-button>
        </a-col>
      </a-row>
    </a-modal>
  </div>

</template>

<script>

import { getUsers, queryUser, getUserDetial, putUserDetial, getUsersCreateFields, delUser } from "@/api/users";
import { encrypt } from "@/utils/jsencrypt"

export default ({
  inject: ["reload"],
  components: {
  },
  data() {
    const checkPassword = (rule, value, callback) => {
      if (value) {
        var reg = /(?!^(\d+|[a-zA-Z]+|[~!@#$%^&*()_.]+)$)^[\w~!@#$%^&*()_.]{8,16}$/
        if (reg.test(value) === false) {
          callback(new Error('密码应为字母，数字，特殊符号(~!@#$%^&*()_.)，两种及以上组合，8-16位字符串，如：zs666@abc'))
        } else {
          callback()
        }
      } else {
        callback()
      }
    }
    return {
      // Salary cards data
      rules: {
        username: [
          // 限制必填
          { required: false, message: '请输入用户名', trigger: 'blur' },
          // 限制字符串长度
          { min: 2, max: 30, message: '长度在 2 到 30 个字符', trigger: 'blur' },
        ],
        password: [
          // 限制必填
          { required: false, message: '请输入密码', trigger: 'blur' },
          // 限制字符串长度
          { min: 8, max: 16, message: '长度在 8 到 16 个字符', trigger: 'blur' },
          // 自定义正则
          { required: true, trigger: 'blur', validator: checkPassword }
        ]
      },
      passwordRequired: true,
      allUsers: [],
      users: [],
      userTotal: 0,
      currentUser: null,
      userNotEdit: true,
      deleteVisible: false,
      willDelUserName: null,
      userPage: {
        page: 1,
        limit: 5,
      },
      userQuery: {
        username: '',
        email: ''
      },
      userLoading: false
    }
  },
  created() {
    this.initData()
  },
  methods: {
    initData() {
      this.getUsers()
      this.queryUsers()
    },
    getUsers() {
      getUsers().then(res => {
        this.allUsers = res.users
      })
    },
    queryUsers() {
      this.userLoading = true
      queryUser(this.userQuery, this.userPage).then(res => {
        if (res && res.data) {
          this.users = res.data.rows
          this.userTotal = res.data.total
          this.userLoading = false
        }
      })
    },
    searchUser() {
      this.userPage.page = 1
      queryUser(this.userQuery, this.userPage).then(res => {
        if (res && res.data) {
          this.users = res.data.rows
          this.userTotal = res.data.total
        }
      })
    },
    pageChange(event) {
      this.userPage.page = event
      this.queryUsers()
    },
    getUserDetial(username) {
      getUserDetial(username).then(res => {

        const roles = res.user.roles

        let roleNameList = ['ADMIN', 'GENERAL', 'ARTIFACTS_MANAGER']
        res.assignableRoles = res.assignableRoles.filter(item => roleNameList.includes(item.name))

        res.assignableRoles.forEach((item) => {
          if (roles.indexOf(item.name) > -1) {
            item.enabled = true
          } else {
            item.enabled = false
          }
        })
        this.currentUser = res
      })
    },
    userEditHandle() {
      this.userNotEdit = false
      this.passwordRequired = false
    },
    userEditSaveHandle() {
      this.$refs.userForm.validate(valid => {
        if (valid) {
          this.currentUser.user.accessModel = { repositoriesAccess: [] }
          this.currentUser.user.authorities = []
          let roles = []
          this.currentUser.assignableRoles.forEach((item) => {
            if (item.enabled) {
              roles.push(item.name)
            }
          })
          if (!roles || roles.length === 0) {
            this.$notification.warning({
              message: "请选择角色",
              description: ""
            })
            return false
          }
          this.currentUser.user.roles = roles
          let user = JSON.parse(JSON.stringify(this.currentUser.user))
          if (user.password) {
            user.password = encrypt(user.password)
          }
          putUserDetial(user).then(res => {
            this.userNotEdit = true
            this.reload()
          })
        } else {
          return false
        }
      })
    },
    userCreate() {
      getUsersCreateFields().then(res => {
        let roles = res.formDataValues[0].values
        if (roles) {
          let roleNameList = ['ADMIN', 'GENERAL', 'ARTIFACTS_MANAGER']
          roles = roles.filter(item => roleNameList.includes(item.name))
          roles.forEach((item) => { item.enabled = false })
        }
        this.currentUser = { user: {}, assignableRoles: roles }
        this.userNotEdit = false
        this.passwordRequired = true
        if (this.$refs.userForm) {
          this.$refs.userForm.resetFields()
        }
      })
    },
    userEditCancelHandle() {
      this.userNotEdit = true
      this.$refs.userForm.resetFields()
    },
    delUserHandle(username) {
      this.willDelUserName = username
      this.deleteVisible = true
    },
    delUser(username) {
      delUser(username).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: '删除成功',
          })
        }, 100)
        this.deleteVisible = false
        this.reload()
      })
    },
    roleLogBuild(role) {
      let roleA = role.toLowerCase();
      return roleA === 'admin' ? 'admin' : roleA === 'anonymous' ? 'anonymous' : roleA === 'artifacts_manager' ? 'artifact_manager' : roleA === 'global_configuration_manager' ? 'global_configuration_manager' : 'other_role'
    },
    getUserCount(role) {
      let adminUserList = this.allUsers
      return adminUserList.filter((i) => { return i.roles.indexOf(role) > -1 })
    }
  }
})

</script>

<style lang="scss">

</style>