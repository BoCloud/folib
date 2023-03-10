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
              <h6>仓库可见范围</h6>
              <p v-if="permissionForm.scope === 1">存储空间成员可见，可拉取</p>
              <p v-else>所有成员可见，可拉取</p>
            </template>
  
            <a-radio-group v-model="permissionForm.scope">
              <a-radio :value="1">
                存储空间
              </a-radio>
              <a-radio :value="2">
                公开
              </a-radio>
            </a-radio-group>
          </a-card>
          <a-card :bordered="false" class="header-solid">
            <template #title>
              <h6>仓库权限定义</h6>
              <p>在此定义用户对于该仓库的制品上传、制品删除权限</p>
            </template>
            <a-radio-group @change="userChange" v-model="userRadioDefault">
              <a-radio :value="1">
                添加用户
              </a-radio>
              <a-radio :value="2">
                选择全部
              </a-radio>
            </a-radio-group>
            <div class="mt-10">
              <a-select
                placeholder="添加用户"
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
            <a-table v-if="permissionForm.userList && permissionForm.userList.length >0" :columns="permissionColumns" :data-source="permissionForm.userList" :pagination="false">
              <template slot="username" slot-scope="text, record">
                <p class="username">{{record.username}}</p>
                <small v-if="record.permissions && record.permissions.length>0">{{'拥有该仓库的' + (record.permissions.length === 2?'上传、删除权限':(record.permissions.includes('ARTIFACTS_DEPLOY')?'上传权限':'删除权限'))}}</small>
              </template>
              <template slot="deploy" slot-scope="text, record">
                <a-switch :checked="record.permissions&&record.permissions.includes('ARTIFACTS_DEPLOY')" @change="deploySwitchChange($event, record)" checked-children="是" un-checked-children="否"/>
              </template>
              <template slot="delete" slot-scope="text, record">
                <a-switch :checked="record.permissions&&record.permissions.includes('ARTIFACTS_DELETE')" @change="deployDeleteChange($event, record)" checked-children="是" un-checked-children="否"/>
              </template>
              <template slot="operation" slot-scope="text, record">
                <a-popconfirm title="确定要删除吗？" okType="danger" ok-text="确定" cancel-text="取消"
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
          </a-card>
          <a-form-model-item :wrapper-col="{ span: 14, offset: 4 }">
            <a-button type="primary" @click="permissionFormSubmit">
              保存
            </a-button>
            <a-button class="ml-10" @click="permissionResetForm">
              取消
            </a-button>
          </a-form-model-item>
        </a-col>
      </a-row>
    </a-form-model>
  </div>
</template>
<script>
import {
  repositoryPermission,
  deleteRepositoryPermission,
} from "@/api/folib"

export default {
  props: { 
		folibRepository: {
			type: Object,
			default: {},
		},
    permissionForm: {
			type: Object,
			default: {
        scope: 1,
        userList: []
      },
		},
    userList: {
			type: Array,
			default: () => [],
		},
    sourceUserList: {
			type: Array,
			default: () => [],
		},
    settingVisible: {
			type: Boolean,
			default: false,
		},
	},
  data() {
    return {
      permissionUserShow: false,
      permissionRules: {
        scope: [
          { required: true, message: '请选择仓库可见范围', trigger: 'blur' },
        ],
      },
      permissionColumns: [
      {
          title: "用户名",
          dataIndex: "username",
          scopedSlots: { customRender: "username" },
          width: 350,
        },
        {
          title: "上传",
          dataIndex: "deploy",
          scopedSlots: { customRender: "deploy" },
          align: "center",
          width: 100,
        },
        {
          title: "删除",
          dataIndex: "delete",
          scopedSlots: { customRender: "delete" },
          align: "center",
          width: 100,
        },
        {
          title: "操作",
          dataIndex: "operation",
          scopedSlots: { customRender: "operation" },
          width: 150,
        },
      ],
      permissionList: [
        {
          label: "上传",
          value: "ARTIFACTS_DEPLOY",
        },
        {
          label: "删除",
          value: "ARTIFACTS_DELETE",
        },
      ],
      userRadioDefault: 0
    }
  },
  components: {
    
  },
  created() {

  },
	watch: {
    settingVisible: function (newval) {
      this.userRadioDefault = 0
    },
  },
  mounted() {},
  methods: {
    successMsg(message) {
      if (!message) {
        message = "操作成功";
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    userChange(e) {
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
    permissionFormSubmit() {
      this.$refs.permissionForm.validate(valid => {
        if (valid) {
          if (this.permissionForm.scope === null) {
            this.$notification["warning"]({
              message: "请选择仓库可见范围",
              description: ""
            })
            return false
          }
          for (let item of this.permissionForm.userList) {
            if (!item.permissions || item.permissions.length < 1) {
              this.$notification["warning"]({
                message: "至少给" + item.username + '赋予一项权限',
                description: ""
              })
              return false
            }
          }
          let data = {
            scope: this.permissionForm.scope,
            userList: this.permissionForm.userList
          }
          repositoryPermission(this.folibRepository.storageId, this.folibRepository.id, data).then(res => {
            this.successMsg("仓库设置成功")
            this.permissionUserShow = false
            this.$emit('settingDrawerClose')
          }).catch((err) => {
            this.$notification["error"]({
              message: err.response.data.message,
              description: ""
            })
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
        this.$notification["warning"]({
          message: "没有满足条件的成员，请先给存储空间分配合适的成员",
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

</style>