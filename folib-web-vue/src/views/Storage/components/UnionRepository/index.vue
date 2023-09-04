<template>
  <div>
    <a-form-model layout="horizontal" ref="unionRepositoryForm" :model="unionRepositoryForm" :rules="unionRepositoryRules"
      :hideRequiredMark="true">
      <a-row :gutter="[24]">
        <a-col :span="24">
          <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: '8px', paddingBottom: '8px', paddingLeft: '16px', paddingRight: '16px' }">
            <template #title>
              <h6>联邦仓库制品晋级</h6>
              <p v-if="unionRepositoryForm.enable === true">联邦仓库制品晋级已开启</p>
              <p v-else>联邦仓库制品晋级已关闭</p>
            </template>
            <a-radio-group v-model="unionRepositoryForm.enable">
              <a-radio :value="true">
                开启&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
              </a-radio>
              <a-radio :value="false">
                关闭
              </a-radio>
            </a-radio-group>
          </a-card>
          <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: '8px', paddingBottom: '8px', paddingLeft: '16px', paddingRight: '16px' }">
            <template #title>
              <h6>晋级规则</h6>
            </template>
            <a-radio-group v-model="unionRepositoryForm.syncType" @change="syncChange">
              <a-radio :value="1">
                <span>制品路径</span>
                <a-popover placement="topLeft">
                  <template slot="content">
                    <p class="mb-0">输入关键词用于匹配制品路径，支持正则表达式，</p>
                    <p class="mb-0">普通示例：example可匹配制品路径中包含example的制品，</p>
                    <p class="mb-0">正则示例：(.*example.*)(.*V1.*)可匹配镜像名包含example，</p>
                    <p class="mb-0">镜像版本包含V1的镜像</p>
                  </template>
                  <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                </a-popover>
              </a-radio>
              <a-radio :value="2">
                <span>元数据</span>
                <a-popover placement="topLeft">
                  <template slot="content">
                    <p class="mb-0">输入元数据KEY及元数据值，字符相等匹配</p>
                    <p class="mb-0">用于匹配制品元数据中包含该KEY及值的制品</p>
                  </template>
                  <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                </a-popover>
              </a-radio>
            </a-radio-group>
          </a-card>
          <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: '8px', paddingBottom: '8px', paddingLeft: '16px', paddingRight: '16px' }">
            <a-row v-if="unionRepositoryForm.syncType === 1">
              <a-col :span="8">
                <a-form-model-item prop="artifactPaths" class="mb-0">
                  <a-input placeholder="请输入包路径" :maxLength="200" v-model="unionRepositoryForm.artifactPaths" />
                </a-form-model-item>
              </a-col>
            </a-row>
            <a-row v-if="unionRepositoryForm.syncType === 2">
              <a-col :span="8">
                <a-form-model-item prop="metadataKey" class="mb-0">
                  <a-input placeholder="请输入元数据KEY" :maxLength="100" v-model="unionRepositoryForm.metadataKey" />
                </a-form-model-item>
                <a-form-model-item prop="metadataValue" class="mt-10 mb-0" >
                  <a-input placeholder="请输入元数据值" :maxLength="200" v-model="unionRepositoryForm.metadataValue"/>
                </a-form-model-item>
              </a-col>
            </a-row>
          </a-card>
          <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: '8px', paddingBottom: '8px', paddingLeft: '16px', paddingRight: '16px' }">
            <template #title>
              <h6>联邦仓库选择</h6>
              <p>选择其他集群的制品仓库组成联邦仓库</p>
            </template>
            <a-radio-group v-model="unionRepositoryForm.artifactoryType" @change="typeChange">
              <a-radio :value="1">
                <span>内部节点</span>
                <a-popover placement="topLeft">
                  <template slot="content">
                    <p class="mb-0">{{instanceName + '制品库节点'}}</p>
                  </template>
                  <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                </a-popover>
              </a-radio>
              <a-radio :value="2" v-if="this.folibRepository.layout === 'Raw'">
                <span>外部节点</span>
                <a-popover placement="topLeft">
                  <template slot="content">
                    <p class="mb-0">其他类型制品库节点</p>
                  </template>
                  <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                </a-popover>
              </a-radio>
            </a-radio-group>
            <a-row class="mt-20" v-if="unionRepositoryForm.artifactoryType === 1">
              <a-col :span="24">
                <a-row>
                  <a-col :span="8">
                    <a-tree-select v-model="selectTargetRepositories" style="width: 100%"
                      :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }" :tree-data="targetRepositories"
                      placeholder="请选择联邦仓库" allow-clear show-search @change="targetRepositoriesChange"
                      :replaceFields="{ children: 'children', title: 'key', key: 'key', value: 'key' }">
                    </a-tree-select>
                  </a-col>
                </a-row>
                <a-table v-if="unionRepositoryForm.unionTargetRepositories && unionRepositoryForm.unionTargetRepositories.length > 0"
                  :columns="targetRepositoriesColumns" :data-source="unionRepositoryForm.unionTargetRepositories" :pagination="false" :scroll="{ x: true }"
                  :row-key="(r, i) => i.toString()">
                  <template slot="operation" slot-scope="text, record, index">
                    <a-popconfirm title="确定要删除吗？" okType="danger" ok-text="确定" cancel-text="取消"
                      @confirm="tableTargetRepositoriesDelete(index, record)">
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
              </a-col>
            </a-row>
            <a-row class="mt-20" v-if="unionRepositoryForm.artifactoryType === 2">
              <a-col :span="24">
                <a-row>
                  <a-col :span="8">
                    <a-tree-select v-model="selectTargetRepositories" style="width: 100%"
                      :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }" :tree-data="externalNodeRepositories"
                      placeholder="请选择联邦仓库" allow-clear show-search @change="targetExternalNodeRepositoriesChange"
                      :replaceFields="{ children: 'repositories', title: 'key', key: 'key', value: 'key' }">
                    </a-tree-select>
                  </a-col>
                </a-row>
                <a-table v-if="unionTargetExternalNodeRepositories && unionTargetExternalNodeRepositories.length > 0"
                  :columns="externalNodeRepositoriesColumns" :data-source="unionTargetExternalNodeRepositories" :pagination="false" :scroll="{ x: true }"
                  :row-key="(r, i) => i.toString()">
                  <template slot="operation" slot-scope="text, record, index">
                    <a-popconfirm title="确定要删除吗？" okType="danger" ok-text="确定" cancel-text="取消"
                      @confirm="tableTargetExternalNodeRepositoriesDelete(index, record)">
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
              </a-col>
            </a-row>
          </a-card>
          <a-form-model-item :wrapper-col="{ span: 14, offset: 4 }">
            <a-button type="primary" @click="unionRepositoryFormSubmit">
              保存
            </a-button>
            <a-button class="ml-10" @click="unionRepositoryFormReset">
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
  getRepositoryResponseEntity,
  getArtifactDispatchStoragesAndRepositories,
  unionRepositoryConfig,
} from "@/api/folib"
import { getExternalNodeRepositories } from "@/api/externalNode"
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
    const artifactPathsValidator = (rule, value, callBack) => {
      if (this.unionRepositoryForm.enable && this.unionRepositoryForm.syncType === 1) {
        if (!value) {
          callBack("请输入包路径")
        } else {
          callBack()
        }
      } else {
        callBack()
      }
    }
    const metadataKeyValidator = (rule, value, callBack) => {
      if (this.unionRepositoryForm.enable && this.unionRepositoryForm.syncType === 2) {
        if (!value) {
          callBack("请输入元数据KEY")
        } else {
          callBack()
        }
      } else {
        callBack()
      }
    }
    const metadataValueValidator = (rule, value, callBack) => {
      if (this.unionRepositoryForm.enable && this.unionRepositoryForm.syncType === 2) {
        if (!value) {
          callBack("请输入元数据值")
        } else {
          callBack()
        }
      } else {
        callBack()
      }
    }
    return {
      targetRepositories: [],
      selectTargetRepositories: [],
      externalNodeRepositories: [],
      unionTargetExternalNodeRepositories: [],
      unionRepositoryForm: {
        enable: false,
        syncType: 1,
        artifactoryType: 1,
        artifactPaths: "",
        metadataKey: "",
        metadataValue: "",
        unionTargetRepositories: [],
      },
      targetRepositoriesColumns: [
        {
          title: "节点",
          dataIndex: "node",
          scopedSlots: { customRender: "node" },
          width: 150,
        },
        {
          title: "存储空间",
          dataIndex: "storageId",
          scopedSlots: { customRender: "storageId" },
          align: "left",
          width: 150,
        },
        {
          title: "仓库名称",
          dataIndex: "repositoryId",
          scopedSlots: { customRender: "repositoryId" },
          align: "left",
          width: 150,
        },
        {
          title: "操作",
          dataIndex: "operation",
          scopedSlots: { customRender: "operation" },
          width: 150,
        },
      ],
      unionRepositoryRules: {
        enable: [
          { required: false, trigger: ['blur', 'change'], message: '请选择是否开启制品晋级', },
        ],
        syncType: [
          { required: false, trigger: ['blur', 'change'], message: '请选择晋级规则', },
        ],
        artifactPaths: [
          { required: false, trigger: ['blur', 'change'], validator: artifactPathsValidator },
          { min: 1, max: 200, message: '包路径长度在1到200个字符', trigger: 'blur' },
        ],
        metadataKey: [
          { required: false, trigger: ['blur', 'change'], validator: metadataKeyValidator },
          { min: 1, max: 100, message: '元数据KEY长度在1到100个字符', trigger: 'blur' },
        ],
        metadataValue: [
          { required: false, trigger: ['blur', 'change'], validator: metadataValueValidator },
          { min: 1, max: 200, message: '元数据值长度在1到200个字符', trigger: 'blur' },
        ],
      },
      instanceName: '',
      externalNodeRepositoriesColumns: [
        {
          title: "节点",
          dataIndex: "node",
          scopedSlots: { customRender: "node" },
          width: 150,
        },
        {
          title: "制品库类型",
          dataIndex: "type",
          scopedSlots: { customRender: "type" },
          align: "left",
          width: 150,
        },
        {
          title: "仓库名称",
          dataIndex: "repositoryId",
          scopedSlots: { customRender: "repositoryId" },
          align: "left",
          width: 150,
        },
        {
          title: "操作",
          dataIndex: "operation",
          scopedSlots: { customRender: "operation" },
          width: 150,
        },
      ],
    }
  },
  components: {

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
  mounted() { },
  methods: {
    initData() {
      this.instanceName = sessionStorage.getItem("instanceName")
      this.repositoryInfo()
      this.getTargetRepositories(this.folibRepository.type, this.folibRepository.layout, this.folibRepository.id, this.folibRepository.policy)
      this.getExternalNodeRepositories()
      this.selectTargetRepositories = []
    },
    repositoryInfo() {
      getRepositoryResponseEntity(this.folibRepository.storageId, this.folibRepository.id).then(res => {
        if (res.id === this.folibRepository.id && res.unionRepositoryConfiguration) {
          let artifactPaths = res.unionRepositoryConfiguration.artifactPaths
          if (artifactPaths && artifactPaths.length > 0) {
            artifactPaths = artifactPaths[0]
          } else {
            artifactPaths = ""
          }
          this.unionRepositoryForm = {
            enable: res.unionRepositoryConfiguration.enable,
            syncType: res.unionRepositoryConfiguration.syncType,
            artifactoryType: 1,
            artifactPaths: artifactPaths,
            metadataKey: res.unionRepositoryConfiguration.metadataKey,
            metadataValue: res.unionRepositoryConfiguration.metadataValue,
          }
          if (res.unionRepositoryConfiguration.unionTargetRepositories) {
            this.unionRepositoryForm.unionTargetRepositories = res.unionRepositoryConfiguration.unionTargetRepositories.filter(item => item.type === 'inner')
            this.unionTargetExternalNodeRepositories = res.unionRepositoryConfiguration.unionTargetRepositories.filter(item => item.type !== 'inner')
          }
        }
      })
    },
    handleCheckboxChange(selectedData) {
    },
    successMsg(message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification["success"]({
        message: message,
        description: "",
      })
    },
    syncChange(e) {
      if (e.target.value === 1) {
        this.$refs.unionRepositoryForm.clearValidate(['metadataKey','metadataValue'])
      } else if (e.target.value === 2) {
        this.$refs.unionRepositoryForm.clearValidate('artifactPaths')
      }
    },
    getTargetRepositories(type, layout, excludeRepositoryId, policy) {
      getArtifactDispatchStoragesAndRepositories({
        type: type,
        layout: layout,
        excludeRepositoryId: excludeRepositoryId,
        policy: policy,
      }).then((res) => {
        this.targetRepositories = []
        res.forEach((item) => {
          if (item.children && item.children.length > 0) {
            this.targetRepositories.push(item)
          }
        })
      })
    },
    targetRepositoriesChange(value, label, extra) {
      if (value) {
        let arr = value.split(",")
        if (arr.length === 3) {
          let target = {
            node: arr[0],
            type: 'inner',
            storageId: arr[1],
            repositoryId: arr[2],
          }
          if (!this.exists(target)) {
            this.unionRepositoryForm.unionTargetRepositories.push(target)
          }
        }
      }
    },
    targetExternalNodeRepositoriesChange(value, label, extra) {
      if (value) {
        let arr = value.split(",")
        if (arr.length === 2) {
          let target = {
            node: arr[0],
            type: this.getExternalNodeType(arr[0]),
            repositoryId: arr[1],
          }
          if (!this.externalNodeExists(target)) {
            this.unionTargetExternalNodeRepositories.push(target)
          }
        }
      }
    },
    exists(target) {
      for (let index = 0; index < this.unionRepositoryForm.unionTargetRepositories.length; index++) {
        let item = this.unionRepositoryForm.unionTargetRepositories[index]
        if (item.node === target.node && item.storageId === target.storageId && item.repositoryId === target.repositoryId) {
          return true
        }
      }
      return false
    },
    externalNodeExists(target) {
      for (let index = 0; index < this.unionTargetExternalNodeRepositories.length; index++) {
        let item = this.unionTargetExternalNodeRepositories[index]
        if (item.node === target.node && item.repositoryId === target.repositoryId) {
          return true
        }
      }
      return false
    },
    typeChange(event) {
      this.selectTargetRepositories = []
    },
    getExternalNodeType(nodeName) {
      return this.externalNodeRepositories.filter(item => item.nodeName === nodeName)[0].type
    },
    tableTargetRepositoriesDelete(index, record) {
      this.unionRepositoryForm.unionTargetRepositories.splice(index, 1)
      this.$forceUpdate()
    },
    tableTargetExternalNodeRepositoriesDelete(index, record) {
      this.unionTargetExternalNodeRepositories.splice(index, 1)
    },
    unionRepositoryFormReset() {
      this.$refs.unionRepositoryForm.resetFields()
      this.$emit('settingDrawerClose')
    },
    unionRepositoryFormSubmit() {
      this.$refs.unionRepositoryForm.validate(valid => {
        if (valid) {
          let data = Object.assign({}, this.unionRepositoryForm)
          if (data.syncType === 1) {
            data.metadataKey = ""
            data.metadataValue = ""
            data.artifactPaths = [data.artifactPaths]
          } else if (data.syncType === 2) {
            data.artifactPaths = []
          }
          if (this.unionTargetExternalNodeRepositories && this.unionTargetExternalNodeRepositories.length > 0) {
            this.unionTargetExternalNodeRepositories.forEach(externalNode => {
              data.unionTargetRepositories.push(externalNode)
            })
          }
          if (data.enable && (!data.unionTargetRepositories || data.unionTargetRepositories.length === 0)) {
            this.$notification.warning({
              message: "请选择联邦仓库",
              description: "",
            })
            return false
          }
          unionRepositoryConfig(this.folibRepository.storageId,this.folibRepository.id,data).then(res => {
            this.successMsg("联邦仓库保存成功")
            setTimeout(() => {
              this.$emit('settingDrawerClose')
            }, 100)
          })
        }
      })
    },
    getExternalNodeRepositories() {
      getExternalNodeRepositories().then(res => {
        if (res) {
          this.externalNodeRepositories = res
        }
      }).finally(() => { 
      })
    },
  },
}
</script>

<style lang="scss" scoped>
</style>