<template>
  <div>
    <a-tag color="#2db7f5">
      已使用 {{ cacheDirectoryUseSize + cacheForm.sizeUnit }}  约占 {{ cacheDirectoryUseProportion }} %
    </a-tag>
    <a-form-model layout="horizontal" ref="cacheForm" :model="cacheForm" :rules="cacheRules" :hideRequiredMark="false">
      <a-row :gutter="[24]">
        <a-col :span="24">
          <a-row :gutter="[24]">
            <a-col :span="12">
              <a-form-model-item class="mb-10" label="开启缓存" :colon="false">
                <a-switch v-model="cacheForm.enabled" />
              </a-form-model-item>
            </a-col>
            <a-col :span="12" :xs="{ span: 5, offset: 7 }">
              <a-form-model-item >
                <a-button type="danger" @click="cacheHandlerConfirm">
                  保存
                </a-button>
                <a-button class="ml-10" @click="cacheHandlerCancel">
                  取消
                </a-button>
                <a-popconfirm :title="'确定要清空缓存目录' + cacheForm.directoryPath + '吗？'" okType="danger" ok-text="确定" cancel-text="取消"
                  @confirm="cleanupCacheDirectory">
                  <a-button class="ml-10" type="danger">
                    清空
                  </a-button>
                </a-popconfirm>
              </a-form-model-item>
            </a-col>
          </a-row>
        </a-col>
        <a-col :span="8">
          <a-row :gutter="[24]">
            <a-col :span="20">
              <a-form-model-item class="mb-10" :colon="false" prop="directoryPath">
                <template slot="label">
                  缓存目录
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">如果是容器化部署，目录则为容器内目录，需要在容器启动时将缓存目录挂载到容器内部的对应目录</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-tree-select v-if="directoryPathShow" v-model="cacheForm.directoryPath"
                  :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }" :tree-data="directoryPaths"
                  placeholder="请选择缓存目录" :load-data="onLoadData" />
              </a-form-model-item>
            </a-col>
          </a-row>
        </a-col>
        <a-col :span="8">
          <a-row :gutter="[24]">
            <a-col :span="20">
              <a-form-model-item class="mb-10" label="缓存容量" :colon="false" prop="size">
                <a-input placeholder="请输入缓存容量" v-model="cacheForm.size">
                  <a-select slot="addonAfter" v-model="cacheForm.sizeUnit" style="width: 60px">
                    <a-select-option value="TB">
                      TB
                    </a-select-option>
                    <a-select-option value="GB">
                      GB
                    </a-select-option>
                    <a-select-option value="MB">
                      MB
                    </a-select-option>
                  </a-select>
                </a-input>
              </a-form-model-item>
            </a-col>
          </a-row>
        </a-col>
        <a-col :span="8">
          <a-row :gutter="[24]">
            <a-col :span="20">
              <a-form-model-item class="mb-10" :colon="false" prop="minSize">
                <template slot="label">
                  单文件最小缓存值
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">大于等于单文件最小缓存值的制品才会被放入缓存中</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-input placeholder="请输入单文件最小缓存值" v-model="cacheForm.minSize">
                  <a-select slot="addonAfter" v-model="cacheForm.minSizeUnit" style="width: 60px">
                    <a-select-option value="KB">
                      KB
                    </a-select-option>
                    <a-select-option value="MB">
                      MB
                    </a-select-option>
                    <a-select-option value="GB">
                      GB
                    </a-select-option>
                  </a-select>
                </a-input>
              </a-form-model-item>
            </a-col>
          </a-row>
        </a-col>
        <a-col :span="8">
          <a-row :gutter="[24]">
            <a-col :span="20">
              <a-form-model-item class="mb-10" :colon="false" prop="maxSize">
                <template slot="label">
                  单文件最大缓存值
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">小于等于单文件最大缓存值的制品才会被放入缓存中</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-input placeholder="请输入单文件最大缓存值" v-model="cacheForm.maxSize">
                  <a-select slot="addonAfter" v-model="cacheForm.maxSizeUnit" style="width: 60px">
                    <a-select-option value="KB">
                      KB
                    </a-select-option>
                    <a-select-option value="MB">
                      MB
                    </a-select-option>
                    <a-select-option value="GB">
                      GB
                    </a-select-option>
                  </a-select>
                </a-input>
              </a-form-model-item>
            </a-col>
          </a-row>
        </a-col>
        <a-col :span="8">
          <a-row :gutter="[24]">
            <a-col :span="20">
              <a-form-model-item class="mb-10" :colon="false" prop="clearCondition">
                <template slot="label">
                  清理条件（百分比）
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">输入1-100的值，例如输入90，表示大于等于缓存容量的90%时开始清理</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-input-number v-model="cacheForm.clearCondition" placeholder="请输入清理条件（百分比）" style="width: 100%" :min="1"
                  :max="100" :precision="0">
                </a-input-number>
              </a-form-model-item>
            </a-col>
          </a-row>
        </a-col>
        <a-col :span="8">
          <a-row :gutter="[24]">
            <a-col :span="20">
              <a-form-model-item class="mb-10" :colon="false" prop="clearProportion">
                <template slot="label">
                  清理比例（百分比）
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">输入1-100的值，例如输入10，表示达到清理条件时，至少清理缓存容量的10%，可能会大于10%</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-input-number v-model="cacheForm.clearProportion" placeholder="请输入清理比例（百分比）" style="width: 100%"
                  :min="1" :max="100" :precision="0">
                </a-input-number>
              </a-form-model-item>
            </a-col>
          </a-row>
        </a-col>
      </a-row>
    </a-form-model>
  </div>
</template>
<script>
import {
  folderList,
  getSingleDict,
  updateSingleDict,
} from "@/api/advanced"
import {
  cleanupArtifactCacheDirectory,
  artifactCacheDirectoryUseSize
} from "@/api/artifactCache"
export default {
  props: {
  },
  components: {

  },
  data() {
    const minSizeValidator = (rule, value, callBack) => {
      if (value && this.cacheForm.maxSize && this.cacheForm.minSizeUnit !== this.cacheForm.maxSizeUnit) {
        callBack("请保持最小缓存值单位与最大缓存值单位一致")
      } else if (this.cacheForm.minSizeUnit === this.cacheForm.maxSizeUnit && new Number(value) >= new Number(this.cacheForm.maxSize)) {
        callBack("最小缓存值需小于最大缓存值")
      } else {
        callBack()
      }
    }
    const maxSizeValidator = (rule, value, callBack) => {
      if (value && this.cacheForm.minSize && this.cacheForm.minSizeUnit !== this.cacheForm.maxSizeUnit) {
        callBack("请保持最大缓存值单位与最小缓存值单位一致")
      } else if (this.cacheForm.minSizeUnit === this.cacheForm.maxSizeUnit && new Number(value) <= new Number(this.cacheForm.minSize)) {
        callBack("最大缓存值需大于最小缓存值")
      } else {
        callBack()
      }
    }
    return {
      cacheRules: {
        directoryPath: [
          { required: true, message: "请选择缓存目录", trigger: ["blur", "change"] },
        ],
        size: [
          { required: true, message: "请输入缓存容量", trigger: "blur" },
          { min: 1, max: 10, message: '缓存容量长度在1到10个字符', trigger: 'blur' },
        ],
        clearCondition: [
          { required: true, message: "请输入清理条件", trigger: ['blur'] },
        ],
        clearProportion: [
          { required: true, message: "请输入清理比例", trigger: ['blur'] },
        ],
        minSize: [
          { required: false, trigger: ['blur'], validator: minSizeValidator }
        ],
        maxSize: [
          { required: false, trigger: ['blur'], validator: maxSizeValidator }
        ],
      },
      cacheForm: {
        enabled: false,
        directoryPath: undefined,
        minSize: undefined,
        minSizeUnit: "KB",
        maxSize: undefined,
        maxSizeUnit: "KB",
        size: undefined,
        sizeUnit: "TB",
        clearCondition: undefined,
        clearProportion: undefined,
      },
      directoryPathShow: true,
      directoryPaths: [],
      cacheKey: "cache_settings",
      cacheDirectoryUseSize: 0.00,
      cacheDirectoryUseProportion: 0.00,
    };
  },
  created() {
    this.init()
  },
  watch: {

  },
  mounted() { },
  methods: {
    alertMsg(type,message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification[type]({
        message: message,
      })
    },
    init() {
      this.folderList()
      this.getCacheSettings()
    },
    folderList() {
      folderList({ directoryPath: "/" }).then(res => {
        this.directoryPaths = []
        if (res) {
          res.forEach(item => {
            this.directoryPaths.push({
              key: item.fullPath,
              title: item.name,
              value: item.fullPath,
              isLeaf: !item.hasSubDirectories
            })
          })
        }
      })
    },
    //异步加载树形数据
    onLoadData(treeNode) {
      return new Promise((resolve) => {
        if (treeNode.dataRef.children) {
          resolve()
          return
        }
        let child = []
        folderList({ directoryPath: treeNode.dataRef.value }).then((res) => {
          if (res) {
            res.forEach((item) => {
              let obj = {}
              obj = {
                key: item.fullPath,
                title: item.name,
                value: item.fullPath,
                isLeaf: !item.hasSubDirectories
              };
              child.push(obj)
            })
            treeNode.dataRef.children = child
            this.directoryPaths = [...this.directoryPaths]
            resolve()
          }
        })
      })
    },
    resetCacheForm() {
      this.cacheForm = {
        enabled: false,
        directoryPath: undefined,
        minSize: undefined,
        minSizeUnit: "KB",
        maxSize: undefined,
        maxSizeUnit: "KB",
        size: undefined,
        sizeUnit: "TB",
        clearCondition: undefined,
        clearProportion: undefined,
      }
      if (this.$refs.cacheForm) {
        this.$refs.cacheForm.resetFields()
        this.getCacheSettings()
      }
    },
    cleanupCacheDirectory() {
      cleanupArtifactCacheDirectory({directoryPath: this.cacheForm.directoryPath}).then(res => {
        this.alertMsg('success','清空缓存目录成功')
        this.directoryPathShow = false
        this.getCacheSettings()
      }).catch(err => {
        this.$notification['error']({
          message: err.response.data.error,
          description: ''
        })
      }).finally(() => {
      })
    },
    artifactCacheDirectoryUseSize() {
      artifactCacheDirectoryUseSize({directoryPath: this.cacheForm.directoryPath, unit: this.cacheForm.sizeUnit}).then(res => {
        if (res >= 0) {
          this.cacheDirectoryUseSize = res
          let useSize = new Number(this.cacheDirectoryUseSize)
          let size = new Number(this.cacheForm.size)
          this.cacheDirectoryUseProportion = ( useSize / size).toFixed(2)
        }
      }).catch(err => {
        this.$notification['error']({
          message: err.response.data.error,
          description: ''
        })
      }).finally(() => {
      })
    },
    cacheHandlerCancel() {
      this.resetCacheForm()
    },
    cacheHandlerConfirm() {
      this.$refs.cacheForm.validate((valid) => {
        if (valid) {
          let data = {
            dictType: this.cacheKey,
            dictKey: this.cacheKey,
            dictValue: JSON.stringify(this.cacheForm),
          }
          updateSingleDict(data).then(res => {
            this.alertMsg('success','缓存策略设置成功')
            this.directoryPathShow = false
            this.getCacheSettings()
          }).catch(err => {
            this.$notification['error']({
              message: err.response.data.error,
              description: ''
            })
          }).finally(() => {
            this.cacheHandlerCancel()
          })
        } else {
          return false
        }
      })
    },
    getCacheSettings() {
      getSingleDict({ dictType: this.cacheKey }).then(res => {
        if (res) {
          if (res.dictValue) {
            this.directoryPathShow = true
            let data = JSON.parse(res.dictValue)
            this.cacheForm = data
            this.artifactCacheDirectoryUseSize()
          }
        }
      })
    },
  },
}
</script>