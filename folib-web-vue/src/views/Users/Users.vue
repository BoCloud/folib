<!--
    This is the Billing page, it uses the dashboard layout in:
    "./layouts/Dashboard.vue" .
 -->

 <template>
  <div class="users">

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
                    <img src="images/folib/userAdmin.svg" alt="userAdmin">
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
                    <img src="images/folib/userAdmin.svg" alt="userAdmin">
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
                    <img src="images/folib/userAdmin.svg" alt="userAdmin">
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
                    <a-col :span="8" class="" :sm="24" :md="12" :lg="12" :xl="currentUser?8:5">
                      <a-input-search  class="v-search" v-model="userQuery.username" :placeholder="$t('Users.EnterTheUsernameQuery')" @search="searchUser()"/>
                    </a-col>
                    <a-col :span="8" class="" :sm="24" :md="12" :lg="12" :xl="currentUser?8:5">
                      <a-input-search class="v-search" v-model="userQuery.email" :placeholder="$t('Users.EnterTheEmailQuery')" @search="searchUser()"/>
                    </a-col>
                    <a-col :span="8" class="" :sm="24" :md="12" :lg="12" :xl="currentUser?8:5">
                          <a-input-search class="v-search" v-model="userQuery.nickname" :placeholder="$t('Users.EnterTheNicknameQuery')" @search="searchUser()"/>
                    </a-col>
                    <a-col :span="8" class="" :sm="24" :md="12" :lg="12" :xl="currentUser?8:5">
                      <a-select
                        class="v-search"
                        v-model="userQuery.userRole"
                        :placeholder="$t('Users.UserRole')"
                        show-search
                        allowClear
                        @change="userRoleChange()"
                        optionFilterProp="value"
                      >
                        <a-select-option
                          v-for="(item) in roleList"
                          :key="item.label"
                          :value="item.value"
                        >
                          {{ $t(item.label) }}
                        </a-select-option>
                      </a-select>
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
                                <a-descriptions-item :label="$t('Users.Nickname')">
                                      {{ item.nickname }}
                                </a-descriptions-item>
                                <a-descriptions-item label="Email">
                                  {{ item.email ? item.email : "" }}
                                </a-descriptions-item>
                                <a-descriptions-item :label="$t('Users.IsItAvailable')">
                                 {{ item.enabled ? $t('Users.Available') : $t('Users.NotAvailable') }}
                                </a-descriptions-item>
                                <a-descriptions-item :label="$t('Users.RoleInformation')">
                                  {{ item.roles }}
                                </a-descriptions-item>
                                <a-descriptions-item :label="$t('Users.GroupInformation')">
                                  {{ item.userGroups }}
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
                <h6 class="font-semibold m-0">{{ userNotEdit ? $t('Users.UserInformation') : isCreate ? $t('Users.Adduser') : $t('Users.UserEdit') }}</h6>
              </template>
              <template slot="extra">
                <div class="col-action mb-0">
                  <a-button type="link" size="small" @click="userEditCancelHandle">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                            d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                            fill="#111827" />
                    </svg>
                    <span class="text-danger">{{ $t('Users.Cancel') }}</span>
                  </a-button>
                  <a-button type="link" size="small" @click="userEditSaveHandle" v-if="!userNotEdit">
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
                        <a-form-model-item class="mb-10" :label="$t('Users.Nickname')" :colon="false">
                            <a-input :disabled="userNotEdit" v-model="currentUser.user.nickname" :placeholder="$t('Users.PleaseEnterNickname')" />
                        </a-form-model-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-model-item class="mb-10" label="EMAIL" :colon="false" name="email" prop="email">
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

import { getUsers, queryUser, getUserDetial, putUserDetial, saveUser, getUsersCreateFields, delUser } from "@/api/users";
import { encrypt } from "@/utils/jsencrypt"
import textOver from "@/components/Tools/textOver";
import { getPermissionList } from "@/api/permissions";
import { getGroupList } from "@/api/group";
import {
  getSingleDict,
} from "@/api/advanced"

export default ({
  inject: ["reload"],
  components: {
    textOver,
  },
  data() {
    const checkPassword = (rule, value, callback) => {
      if (value) {
        var reg = /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[~!@#$%^&*()_.])[A-Za-z\d~!@#$%^&*()_.]{12,30}$|^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.{12,30}$)|^(?=.*[a-z])(?=.*[A-Z])(?=.*[~!@#$%^&*()_.])(?=.{12,30}$)|^(?=.*[a-z])(?=.*\d)(?=.*[~!@#$%^&*()_.])(?=.{12,30}$)|^(?=.*[A-Z])(?=.*\d)(?=.*[~!@#$%^&*()_.])(?=.{12,30}$)/
        if (reg.test(value) === false) {
          callback(new Error(this.$t('Users.PasswordFormat')))
        } else if (value.length < 12 || value.length > 30) {
          callback(new Error(this.$t('Users.PasswordLength')))
        } else {
          callback()
        }
      } else if (!value) {
        if (this.passwordRequired) {
          callback(new Error(this.$t('Users.EnterThePassword')))
        } else {
          callback()
        }
      } else {
        callback()
      }
    }
    const checkUsername = (rule, value, callback) => {
      if (value) {
        if (value.length < 2 || value.length > 36) {
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
          { required: this.passwordRequired, trigger: 'blur', validator: checkPassword }
        ],
        email: [
          { required: false, message: `${this.$t('Users.EnterYourEmail')}`, trigger: 'blur' },
          { type: 'email', message: `${this.$t('Users.EnterCorrectEmail')}`, trigger: 'blur' },
        ],
      },
      passwordRequired: true,
      allUsers: [],
      users: [],
      userTotal: 0,
      currentUser: null,
      userNotEdit: true,
      isCreate: false,
      type: null,
      deleteVisible: false,
      willDelUserName: null,
      userPage: {
        page: 1,
        limit: 5,
      },
      userQuery: {
        username: '',
        email: '',
        userRole: undefined,
        roles: [],
      },
      userLoading: false,
      defaultRoles: [],
      groupList: [],
      roleList: [
        {
          label: "Users.Administrators",
          value: "ADMIN",
        },
        {
          label: "Users.GeneralUsers",
          value: "GENERAL",
        }
      ],
    }
  },
  created() {
    this.initData()
  },
  methods: {
    async initData() {
      await this.getCurrentDefaultRole()
      await this.getCurrentGroup()
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
    userRoleChange() {
      this.userQuery.roles = []
      if(this.userQuery.userRole) {
        this.userQuery.roles.push(this.userQuery.userRole)
      }
      this.searchUser()
    },
    pageChange(event) {
      this.userPage.page = event
      this.queryUsers()
    },
    getUserDetial(username) {
      this.isCreate = false
      getUserDetial(username).then(res => {

        const roles = res.user.roles

        res.assignableRoles = res.assignableRoles.filter(item => this.defaultRoles.includes(item.name))

        res.assignableRoles.forEach((item) => {
          item.enabled = roles.indexOf(item.name) > -1;
        })
        const userGroupIds = res.user.userGroupIds || []
        this.groupList.forEach(item => {
          this.$set(item, 'enabled', userGroupIds.indexOf(`${item.id}`) > -1)
        })
        this.currentUser = res
      })
    },
    async getCurrentDefaultRole() {
      await new Promise((resolve, reject) => {
        getPermissionList({
          page: 1,
          limit: 10,
          isDefault: 1
        }).then(res => {
          this.defaultRoles = []
          if (res && res.data) {
            this.defaultRoles = res.data.rows.map(item => item.id)
          }
          resolve()
        }).catch(e => {
            reject(e)
        })
      })
    },
    async getCurrentGroup() {
      await new Promise((resolve, reject) => {
        getGroupList({
          page: 1,
          limit: 999
        }).then(res => {
          if (res && res.data) {
            this.groupList = res.data.rows
          }
          resolve()
        }).catch(e => {
          reject(e)
        })
      })
    },
    userEditHandle() {
      this.isCreate = false
      this.userNotEdit = false
      this.passwordRequired = false
      if (this.$refs.userForm) {
        this.$refs.userForm.resetFields()
      }
      this.type = 2
    },
    userEditSaveHandle() {
      this.$refs.userForm.validate(valid => {
        if (valid) {
          this.currentUser.user.accessModel = { repositoriesAccess: [] }
          this.currentUser.user.authorities = []
          this.currentUser.user.userGroupIds = []
          let roles = []
          this.currentUser.assignableRoles.forEach((item) => {
            if (item.enabled) {
              roles.push(item.name)
            }
          })
          this.groupList.forEach((item) => {
            if (item.enabled) {
              this.currentUser.user.userGroupIds.push(`${item.id}`)
            }
          })
          if (!roles.length && !this.currentUser.user.userGroupIds.length ) {
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
          if (this.type == 1) {
            saveUser(user).then(res => {
              this.isCreate = false
              this.userNotEdit = true
              this.type = null
              this.reload()
            }).catch((err) => {
              let msg = err.response.data.message ? err.response.data.message : err.response.data
              let errStatusArr = [200, 500, 403, 304, 401]
              if (!errStatusArr.includes(err.response.status)) {
                this.$notification.error({
                  message: msg,
                })
              }
            })
          } else {
            putUserDetial(user).then(res => {
              this.isCreate = false
              this.userNotEdit = true
              this.type = null
              this.reload()
            }).catch((err) => {
              let msg = err.response.data.message ? err.response.data.message : err.response.data
              let errStatusArr = [200, 500, 403, 304, 401]
              if (!errStatusArr.includes(err.response.status)) {
                this.$notification.error({
                  message: msg,
                })
              }
            })
          }
        } else {
          return false
        }
      })
    },
    userCreate() {
      getUsersCreateFields().then(res => {
        let roles = res.formDataValues[0].values
        if (roles) {
          roles = roles.filter(item => this.defaultRoles.includes(item.name))
          roles.forEach((item) => { item.enabled = false })
        }
        const userGroupIds = []
        this.groupList.forEach(item => {
          this.$set(item, 'enabled', item.joinGroup === '1')
          if (item.joinGroup === '1') { userGroupIds.push(`${item.id}`) }
        })
        this.currentUser = { user: { userGroupIds, username:null}, assignableRoles: roles }
        this.isCreate = true
        this.userNotEdit = false
        this.passwordRequired = true
        if (this.$refs.userForm) {
          this.$refs.userForm.resetFields()
        }
      })
      this.type = 1
    },
    userEditCancelHandle() {
      this.isCreate = false
      this.userNotEdit = true
      this.currentUser = null
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
    },
    groupChange(val, id) {
      if (val && !this.currentUser.user.userGroupIds.includes(id)) {
          this.currentUser.user.userGroupIds.push(id)
      } else {
          this.currentUser.user.userGroupIds = this.currentUser.user.userGroupIds.filter(item => item !== id)
      }
    },
  }
})

</script>

<style lang="scss" scoped>
.en-number {font-size:16px;color: #141414;font-weight: 600;}

.group-list {
    max-height: 888px;
    overflow-y: auto;
}
.users {
  .en-number {font-size:16px;color: #141414;font-weight: 600;}

  .v-search {
    width: 240px;
  }
}
</style>
