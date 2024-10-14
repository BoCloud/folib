<template>
  <div>
    <a-form-model
      layout="horizontal"
      ref="permissionForm"
      :model="permissionForm"
      :rules="permissionRules"
      :hideRequiredMark="true"
    >
      <a-row :gutter="[24]">
        <a-col :span="24">
          <a-card :bordered="false" class="header-solid">
            <template #title>
              <h6>{{ $t('Permission.AllowAccess') }}</h6>
              <p v-if="permissionForm.allowAnonymous === true">{{ $t('Permission.AnonymousPull') }}</p>
              <p v-else>{{ $t('Permission.CannotPullArtifacts') }}</p>
            </template>

            <a-radio-group v-model="permissionForm.allowAnonymous">
              <a-radio :value="true">
                {{ $t('Permission.On') }}
              </a-radio>
              <a-radio :value="false">
                {{ $t('Permission.Off') }}
              </a-radio>
            </a-radio-group>
          </a-card>
          <a-card :bordered="false" class="header-solid">
            <template #title>
              <h6>{{ $t('Permission.VisibilityRange') }}</h6>
              <p v-if="permissionForm.scope === 1">{{ $t('Permission.StorageMembers') }}</p>
              <p v-else>{{ $t('Permission.AllMembers') }}</p>
            </template>

            <a-radio-group v-model="permissionForm.scope" @change="scopeChange">
              <a-radio :value="1">
                {{ $t('Permission.StorageSpace') }}
              </a-radio>
              <a-radio :value="2">
                {{ $t('Permission.Public') }}
              </a-radio>
            </a-radio-group>
          </a-card>
<!--          <a-card :bordered="false" class="header-solid" v-if="this.folibRepository.type !== 'group'">
            <template #title>
              <h6>{{ $t('Permission.PublicPermissionDefinition') }}</h6>
              <p>{{ $t('Permission.SpecificAuthority') }}</p>
            </template>
            <a-radio-group @change="userChange" v-model="userRadioDefault">
              <a-radio :value="1">
                {{ $t('Permission.AddUser') }}
              </a-radio>
              <a-radio :value="2">
                {{ $t('Permission.SelectAll') }}
              </a-radio>
            </a-radio-group>
            <div class="mt-10">
              <a-select
                :placeholder="$t('Permission.AddUser')"
                show-search
                allowClear
                v-if="permissionUserShow"
                style="width: 180px"
                @change="permissionUserChange"
                optionFilterProp="value"
              >
                <a-select-option
                  v-for="(item, index) in userList"
                  :key="index"
                  :value="item"
                >
                  {{ item }}
                </a-select-option>
              </a-select>
            </div>
            <a-table v-if="permissionForm.userList && permissionForm.userList.length >0" :columns="i18nPermissionColumns" :data-source="permissionForm.userList" :scroll="{ x: true }" :pagination="false" rowKwy="username">
              <template slot="customTitle">
                {{ $t('Permission.Path') }}
                <a-popover placement="topLeft">
                  <template slot="content">
                    <p class="mb-0">{{ $t('Permission.NoPath') }}</p>
                    <p class="mb-0">{{ $t('Permission.HavePath') }}</p>
                    <p class="mb-0">{{ $t('Permission.PathFormat') }}</p>
                    <p class="mb-0">{{ $t('Permission.ExamplePath') }}</p>
                  </template>
                  <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                </a-popover>
              </template>
              <template slot="username" slot-scope="text, record">
                <p class="username">{{record.username}}</p>
                <small v-if="record.permissions && record.permissions.length>0">{{ $t('Permission.havePermissions') + (record.permissions.length === 2?$t('Permission.UploadAndDelete'):(record.permissions.includes('ARTIFACTS_DEPLOY')?$t('Permission.UploadPermissions'):$t('Permission.DeletePermissions')))}}</small>
              </template>
              <template slot="paths" slot-scope="text, record">
                <a-textarea class="description" :rows="4" v-model="record.paths" :placeholder="$t('Permission.EnterPath')"/>
              </template>
              <template slot="deploy" slot-scope="text, record">
                <a-switch :checked="record.permissions&&record.permissions.includes('ARTIFACTS_DEPLOY')" @change="deploySwitchChange($event, record)" :checked-children="$t('Permission.Yes')" :un-checked-children="$t('Permission.No')"/>
              </template>
              <template slot="delete" slot-scope="text, record">
                <a-switch :checked="record.permissions&&record.permissions.includes('ARTIFACTS_DELETE')" @change="deployDeleteChange($event, record)" :checked-children="$t('Permission.Yes')" :un-checked-children="$t('Permission.No')"/>
              </template>
              <template slot="operation" slot-scope="text, record">
                <a-popconfirm :title="$t('Permission.SureDelete')" okType="danger" :ok-text="$t('Permission.Confirm')" :cancel-text="$t('Permission.Cancel')"
                    @confirm="permissionUserDelete(record)">
                    <a-button type="link" size="small">
                      <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                        <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                          d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                          fill="#111827" />
                      </svg>
                      <span class="text-danger">DELETE</span>
                    </a-button>
                  </a-popconfirm>
              </template>
            </a-table>
          </a-card>-->
          <a-form-model-item :wrapper-col="{ span: 14, offset: 4 }">
            <a-button type="primary" @click="permissionFormSubmit">
              {{ $t('Permission.Save') }}
            </a-button>
            <a-button class="ml-10" @click="permissionResetForm">
              {{ $t('Permission.Cancel') }}
            </a-button>
          </a-form-model-item>
        </a-col>
      </a-row>
    </a-form-model>
  </div>
</template>
<script>
import {
  repositoryEnableUsers,
  repositoryPermission,
  deleteRepositoryPermission,
  getLibraryFilter,
  getRepositoryPermission
} from "@/api/folib"

export default {
  props: {
		folibRepository: {
			type: Object,
			default: {},
    },
    settingVisible: {
			type: Boolean,
			default: false,
		},
	},
  data() {
    const checkScope = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Permission.SelectRepositoryVisibility')))
      }
    }
    return {
      permissionUserShow: false,
      permissionForm: {
        allowAnonymous: true,
        scope: 1,
        userList: []
		  },
      userList: [],
      sourceUserList: [],
      permissionRules: {
        scope: [
          { required: true,  trigger: 'blur', validator: checkScope },
        ],
      },
      permissionColumns: [
        {
          title: "用户名",
          i18nKey: 'Permission.UserName',
          dataIndex: "username",
          scopedSlots: { customRender: "username" },
          width: 150,
        },
        {
          dataIndex: "paths",
          slots: { title: 'customTitle' },
          scopedSlots: { customRender: "paths" },
          width: 250,
        },
        {
          title: "上传",
          i18nKey: 'Permission.Upload',
          dataIndex: "deploy",
          scopedSlots: { customRender: "deploy" },
          align: "center",
          width: 100,
        },
        {
          title: "删除",
          i18nKey: 'Permission.Delete',
          dataIndex: "delete",
          scopedSlots: { customRender: "delete" },
          align: "center",
          width: 100,
        },
        {
          title: "操作",
          i18nKey: 'Permission.Operation',
          dataIndex: "operation",
          scopedSlots: { customRender: "operation" },
          width: 150,
        },
      ],
      permissionList: [
        {
          label: "上传",
          i18nKey: 'Permission.Upload',
          value: "ARTIFACTS_DEPLOY",
        },
        {
          label: "删除",
          i18nKey: 'Permission.Delete',
          value: "ARTIFACTS_DELETE",
        },
      ],
      userRadioDefault: 0,
      storageUsers: [],
    }
  },
  components: {

  },
  computed: {
    i18nPermissionColumns() {
      return this.permissionColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      });
    },
    groupRepositoriesString() {
      return this.folibRepository.groupRepositories && this.folibRepository.groupRepositories.length > 0
        ? this.folibRepository.groupRepositories.join(' ')
        : '';
    }
  },
  created() {
    this.initData()
  },
	watch: {
    settingVisible: function (newval) {
      if (newval) {
        this.initData()
      }
    },
  },
  mounted() {},
  methods: {
    initData () {
      this.queryRepositoryPermission()
      this.getUsersList()
      this.getStorage()
      this.userRadioDefault = 0
      if (this.folibRepository.layout !== 'Raw' && this.folibRepository.layout !== 'Maven 2') {
        this.permissionColumns.splice(this.permissionColumns.findIndex((item) => item.dataIndex === 'paths'), 1)
      }
    },
    successMsg(message) {
      if (!message) {
        message = this.$t('Permission.OperationSuccessful');
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    getUsersList() {
      this.userList = []
      let query = {storageId: this.folibRepository.storageId, repositoryId: this.folibRepository.id, scope: this.permissionForm.scope}
      repositoryEnableUsers(query).then(res => {
        this.userList = res
      })
    },
    queryRepositoryPermission() {
      getRepositoryPermission({storageId: this.folibRepository.storageId, repositoryId: this.folibRepository.id}).then(res => {
        this.permissionForm.scope = res.scope
        this.permissionForm.allowAnonymous = res.allowAnonymous
        this.permissionForm.userList = []
        this.sourceUserList = []
        if (res.userList && res.userList.length > 0) {
          this.permissionForm.userList = res.userList
          this.sourceUserList = Object.assign([], res.userList)
        }
      })
    },
    userReset() {
      this.permissionForm.userList = Object.assign([], this.sourceUserList)
    },
    scopeChange (e) {
      this.userReset()
      this.getUsersList()
    },
    userChange(e) {
      this.userReset()
      let val = e.target.value
      if (val === 1) {
        this.permissionUserShow = true
      } else if (val === 2) {
        this.permissionUserShow = false
        this.permissionStorageUser()
      }
    },
    deploySwitchChange(event,record) {
      let deploy = "ARTIFACTS_DEPLOY"
      if (event) {
        if (!record.permissions.includes(deploy)) {
          record.permissions.push(deploy)
        }
      } else {
        let index = record.permissions.indexOf(deploy)
        if (index !== -1) {
          record.permissions.splice(index, 1)
        }
      }
    },
    deployDeleteChange(event,record) {
      let del = "ARTIFACTS_DELETE"
      if (event) {
        if (!record.permissions.includes(del)) {
          record.permissions.push(del)
        }
      } else {
        let index = record.permissions.indexOf(del)
        if (index !== -1) {
          record.permissions.splice(index, 1)
        }
      }
    },
    getStorage() {
      getLibraryFilter(this.folibRepository.storageId).then(response => {
        this.storageUsers = []
        if (response.users) {
          this.storageUsers = response.users
        }
      })
    },
    permissionFormSubmit() {
      this.$refs.permissionForm.validate(valid => {
        if (valid) {
          if (this.permissionForm.scope === null) {
            this.$notification.warning({
              message: this.$t('Permission.SelectRepositoryVisibility'),
              description: ""
            })
            return false
          }
          /*let userData = this.permissionForm.userList
          for (let item of userData) {
            if (this.permissionForm.scope === 1 && !this.storageUsers.includes(item.username)) {
              this.$notification.warning({
                message: this.$t('Permission.ModifyScope') + item.username + this.$t('Permission.removedUser'),
                description: ""
              })
              return false
            }
            if (!item.permissions || item.permissions.length < 1) {
              this.$notification.warning({
                message: this.$t('Permission.Give') + item.username + this.$t('Permission.permission'),
                description: ""
              })
              return false
            }
            if (item.paths == "") {
              item.paths = null
            }
            if (item.paths) {
              item.paths = item.paths.split(",")
            }
          }*/
          let data = {
            scope: this.permissionForm.scope,
            allowAnonymous: this.permissionForm.allowAnonymous,
           /* userList: userData*/
          }
          repositoryPermission(this.folibRepository.storageId, this.folibRepository.id, data).then(res => {
            this.successMsg(this.$t('Permission.OperationSuccessful'))
            this.permissionUserShow = false
            this.$emit('settingDrawerClose')
          }).catch((err) => {
            let msg = err.response.data.message?err.response.data.message:err.response.data
            if (msg && msg.length > 0) {
              this.$notification.error({
                message: msg,
                description: ""
              })
            }
          })
        }
      })
    },
    permissionResetForm() {
      this.$refs.permissionForm.resetFields()
      this.permissionForm.userList = []
      this.permissionUserShow = false
      this.$emit('settingDrawerClose')
    },
    permissionUserChange(value) {
      if (!value) {
        return
      }
      let userList = this.permissionForm.userList.filter(item => item.username === value)
      if (!userList || userList.length === 0) {
        this.permissionForm.userList.push({
          username: value,
          permissions: []
        })
      }
    },
    permissionStorageUser() {
      if (!this.userList || this.userList.length === 0) {
        this.$notification.warning({
          message: this.$t('Permission.AssigningMembers'),
          description: "",
        })
        return
      }
      this.userList.forEach(username => {
        let userList = this.permissionForm.userList.filter(item => item.username === username)
        if (!userList || userList.length === 0) {
          this.permissionForm.userList.push({
            username: username,
            permissions: []
          })
        }
      })
    },
    permissionUserDelete(user) {
      let index = this.permissionForm.userList.findIndex(item => {
            if (item.username === user.username) {
                return true
            }
      })
      if (index >= 0) {
        let sourceIndex = this.sourceUserList.findIndex(item => {
          if (item.username === user.username) {
              return true
          }
        })
        if (sourceIndex >= 0) {
          deleteRepositoryPermission({storageId: this.folibRepository.storageId, repositoryId: this.folibRepository.id, username: user.username, permissions: user.permissions.join(",")}).then(res => {
            this.permissionForm.userList.splice(index, 1)
            this.sourceUserList.splice(index, 1)
          })
        } else {
          this.permissionForm.userList.splice(index, 1)
        }
      }
    },
  },
};
</script>

<style lang="scss" scoped>
.username{
  color: #141414;
  margin-bottom: 2px;
}

.group-repositories {
  margin-top: 10px;
  padding: 10px;
  background-color: #f0f2f5;
  border-radius: 4px;
}
</style>