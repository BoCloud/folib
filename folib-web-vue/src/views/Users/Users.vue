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
                    <h6 class="font-semibold m-0">{{ $t('Users.UserManagement') }}</h6>
                  </a-col>
                  <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                    <a-button type="primary" @click="userCreate()">
                      {{ $t('Users.Adduser') }}
                    </a-button>
                  </a-col>
                </a-row>
              </template>
              <a-row :gutter="[24, 24]">
                <a-col :span="24" :md="8">
                  <a-card class="payment-method-card">
                    <img src="images/folib/userAdmin.svg" alt="">
                    <div v-if="currentUser"  class="en-number">
                      <textOver
                          :text="$t('Users.TotalNumberOfUsers')"
                          :max="8" />
                    </div>
                    <h6 v-else class="card-number" style="padding-left: 5px">{{ $t('Users.TotalNumberOfUsers') }}</h6>
                    <a-button type="link">
                      <h6 class="card-number">{{ allUsers.length }}</h6>
                    </a-button>
                  </a-card>
                </a-col>
                <a-col :span="24" :md="8">
                  <a-card class="payment-method-card">
                    <img src="images/folib/userAdmin.svg" alt="">
                    <div v-if="currentUser"  class="en-number">
                      <textOver
                          :text="$t('Users.NumberOfAdministrators')"
                          :max="8" />
                    </div>
                    <h6 v-else class="card-number" style="padding-left: 5px">{{ $t('Users.NumberOfAdministrators') }}</h6>
                    <a-button type="link">
                      <h6 class="card-number">{{ getUserCount("ADMIN").length }}</h6>
                    </a-button>
                  </a-card>
                </a-col>
                <a-col :span="24" :md="8">
                  <a-card class="payment-method-card">
                    <img src="images/folib/userAdmin.svg" alt="">
                    <div v-if="currentUser" class="en-number">
                      <textOver
                          :text="$t('Users.NumberOfAnonymousUsers')"
                          :max="8" />
                    </div>
                    <h6 v-else class="card-number" style="padding-left: 5px">{{ $t('Users.NumberOfAnonymousUsers') }}</h6>
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
                <h6 class="font-semibold m-0">{{ $t('Users.UserList') }}</h6>
              </template>
              <a-row :gutter="[24, 24]">
                <a-col :span="24" class="ml-20">
                  <a-row :gutter="[24, 24]">
                    <a-col :span="3" class="">
                      <a-input-search v-model="userQuery.username" :placeholder="$t('Users.EnterTheUsernameQuery')" @search="searchUser()"/>
                    </a-col>
                    <a-col :span="3" class="ml-10">
                      <a-input-search v-model="userQuery.email" :placeholder="$t('Users.EnterTheEmailQuery')" @search="searchUser()"/>
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
                                <a-descriptions-item :label="$t('Users.IsItAvailable')">
                                  {{ item.enabled ? $t('Users.Available') : $t('Users.NotAvailable') }}
                                </a-descriptions-item>
                                <a-descriptions-item label="Email">
                                  {{ item.email ? item.email : "" }}
                                </a-descriptions-item>
                                <a-descriptions-item :label="$t('Users.RoleInformation')">
                                  {{ item.roles }}
                                </a-descriptions-item>
                              </a-descriptions>
                            </div>
                            <div class="col-action">
                              <a-button type="link" size="small" v-if="item.username !== 'admin'" @click.stop="delUserHandle(item.username)">
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
                <h6 class="font-semibold m-0">{{ userNotEdit ? $t('Users.UserInformation') : $t('Users.UserEdit') }}</h6>
              </template>
              <template slot="extra" class="mb-0" v-if="!userNotEdit">
                <div class="col-action">
                  <a-button type="link" size="small" @click="userEditCancelHandle">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                            d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                            fill="#111827" />
                    </svg>
                    <span class="text-danger">{{ $t('Users.Cancel') }}</span>
                  </a-button>
                  <a-button type="link" size="small" @click="userEditSaveHandle">
                    <a-icon type="save" theme="twoTone" />
                    <span class="text-dark">{{ $t('Users.Save') }}</span>
                  </a-button>
                </div>
              </template>
              <a-form-model ref="userForm" :model="currentUser.user" :rules="rules" :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" :label="$t('Users.UserName')" :colon="false" prop="username" :required="true">
                      <a-input :disabled="userNotEdit" :placeholder="$t('Users.EnterYourUsername')" v-model="currentUser.user.username" />
                    </a-form-model-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" :label="$t('Users.Password')" :colon="false" prop="password"
                                       :required="passwordRequired">
                      <a-input-password :disabled="userNotEdit" autocomplete="new-password" placeholder="******"
                                        v-model="currentUser.user.password" />
                    </a-form-model-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" label="EMAIL" :colon="false">
                      <a-input :disabled="userNotEdit" v-model="currentUser.user.email" :placeholder="$t('Users.PleaseEnterEmail')" />
                    </a-form-model-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" :label="$t('Users.ActivatedOrNot')" :colon="false">
                      <span class="mr-15">{{ true ? $t('Users.TurnOn') : $t('Users.ShutDown') }}</span>
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
                <h6 class="font-semibold m-0">{{ userNotEdit ? $t('Users.RoleInformation') : $t('Users.RoleEditing') }}</h6>
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
                    <span class="mr-15">{{ item.enabled ? $t('Users.TurnOn') : $t('Users.ShutDown') }}</span>
                    <a-switch :disabled="userNotEdit" default-checked v-model="item.enabled" @change="roleChange(index)" />
                  </a-col>
                </a-row>
              </div>
            </a-card>
          </a-col>
        </a-row>
      </a-col>
    </a-row>
    <a-modal v-model="deleteVisible" :title="$t('Users.DeleteConfirmation')" :footer="null" :forceRender="true" on-back="deleteVisible = false">

      <a-row :gutter="[24]">
        <a-col :span="24">
          <h6 v-if="deleteVisible" class="text-center font-regular">{{ $t('Users.SureDeleteUser1') }}<a>{{ willDelUserName }}</a>{{ $t('Users.SureDeleteUser2') }}</h6>
        </a-col>
        <a-col :span="24" class="text-right">
          <a-button key="submit" class="px-30" size="small" type="danger" @click="delUser(willDelUserName)">{{ $t('Users.Delete') }}
          </a-button>
          <a-button key="back" @click="deleteVisible = false" class="px-30 ml-10" size="small">{{ $t('Users.Cancel') }}</a-button>
        </a-col>
      </a-row>
    </a-modal>
  </div>

</template>

<script>

import { getUsers, queryUser, getUserDetial, putUserDetial, getUsersCreateFields, delUser } from "@/api/users";
import { encrypt } from "@/utils/jsencrypt"
import textOver from "@/components/Tools/textOver";

export default ({
  inject: ["reload"],
  components: {
    textOver,
  },
  data() {
    const checkPassword = (rule, value, callback) => {
      if (value) {
        var reg = /(?!^(\d+|[a-zA-Z]+|[~!@#$%^&*()_.]+)$)^[\w~!@#$%^&*()_.]{8,16}$/
        if (reg.test(value) === false) {
          callback(new Error(this.$t('Users.passwordFormat')))
        } else if (value.length < 8 || value.length > 16) {
          callback(new Error(this.$t('Users.passwordLength')))
        } else {
          callback()
        }
      } else if (!value) {
        callback(new Error(this.$t('Users.EnterThePassword')))
      } else {
        callback()
      }
    }
    const checkUsername = (rule, value, callback) => {
      if (value) {
        if (value.length < 2 || value.length > 30) {
          callback(new Error(this.$t('Users.userNameLength')))
        } else {
          callback()
        }
      } else if (!value) {
        callback(new Error(this.$t('Users.EnterYourUsername')))
      } else {
        callback()
      }
    }
    return {
      // Salary cards data
      rules: {
        username: [
          { required: true, trigger: 'blur', validator: checkUsername }
        ],
        password: [
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

        let roleNameList = ['ADMIN', 'GENERAL', 'ARTIFACTS_MANAGER', 'OPEN_SOURCE_MANAGE']
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
              message: this.$t('Users.PleaseSelectTheRole'),
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
          let roleNameList = ['ADMIN', 'GENERAL', 'ARTIFACTS_MANAGER', 'OPEN_SOURCE_MANAGE']
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
            message: this.$t('Users.DeleteSuccess'),
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
    },
    roleChange(index) {
      if (this.currentUser.assignableRoles) {
        this.currentUser.assignableRoles.forEach((item,i) => {
          if (i !== index) {
            item.enabled = false
          }
        })
      }
    }
  }
})

</script>

<style lang="scss">
.en-number {font-size:16px;color: #141414;font-weight: 600;}
</style>
