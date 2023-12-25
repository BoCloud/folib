<template>
  <div>
    <a-tag color="#2db7f5">
      {{ $t('Cache.HasBeenUsed') }} {{ cacheDirectoryUseSize + cacheForm.sizeUnit }} {{ $t('Cache.AboutPercentageOf') }} {{ cacheDirectoryUseProportion }} %
    </a-tag>
    <a-form-model layout="horizontal" ref="cacheForm" :model="cacheForm" :rules="cacheRules" :hideRequiredMark="false">
      <a-row :gutter="[24]">
        <a-col :span="24">
          <a-row :gutter="[24]">
            <a-col :span="12">
              <a-form-model-item class="mb-10" :label="$t('Cache.EnableCaching')" :colon="false">
                <a-switch v-model="cacheForm.enabled" />
              </a-form-model-item>
            </a-col>
            <a-col :span="12" :xs="{ span: 5, offset: 7 }">
              <a-form-model-item >
                <a-button type="danger" @click="cacheHandlerConfirm">
                  {{ $t('Cache.Save') }}
                </a-button>
                <a-button class="ml-10" @click="cacheHandlerCancel">
                  {{ $t('Cache.Cancel') }}
                </a-button>
                <a-popconfirm :title="$t('Cache.sureClearCacheDirectory') + cacheForm.directoryPath + $t('Cache.cacheDirectory')" okType="danger" :ok-text="$t('Cache.BeSure')" :cancel-text="$t('Cache.Cancel')"
                  @confirm="cleanupCacheDirectory">
                  <a-button class="ml-10" type="danger">
                    {{ $t('Cache.Empty') }}
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
                  {{ $t('Cache.CacheDirectories') }}
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">{{ $t('Cache.containerizedDeployment') }}</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-tree-select v-if="directoryPathShow" v-model="cacheForm.directoryPath"
                  :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }" :tree-data="directoryPaths"
                  :placeholder="$t('Cache.selectCacheDirectory')" :load-data="onLoadData" />
              </a-form-model-item>
            </a-col>
          </a-row>
        </a-col>
        <a-col :span="8">
          <a-row :gutter="[24]">
            <a-col :span="20">
              <a-form-model-item class="mb-10" :label="$t('Cache.CacheCapacity')" :colon="false" prop="size">
                <a-input :placeholder="$t('Cache.enterCacheCapacity')" v-model="cacheForm.size">
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
                  {{ $t('Cache.MinimumCacheValuePerFile') }}
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">{{ $t('Cache.GreaterThanOrEqualToMinimumCacheValue') }}</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-input :placeholder="$t('Cache.enterMinimumCacheValue')" v-model="cacheForm.minSize">
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
                  {{ $t('Cache.MaximumCacheValueForSingleFile') }}
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">{{ $t('Cache.LessThanOrEqualMaximumCacheValue') }}</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-input :placeholder="$t('Cache.enterMaximumCacheValue')" v-model="cacheForm.maxSize">
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
                  {{ $t('Cache.CleaningCondition') }}
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">{{ $t('Cache.EnterCleaningCapacity') }}</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-input-number v-model="cacheForm.clearCondition" :placeholder="$t('Cache.enterCleaningCondition')" style="width: 100%" :min="1"
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
                  {{ $t('Cache.CleanupRatio') }}
                  <a-popover placement="topLeft">
                    <template slot="content">
                      <p class="mb-0">{{ $t('Cache.AtLeastTheClearedCacheCapacity') }}</p>
                    </template>
                    <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                  </a-popover>
                </template>
                <a-input-number v-model="cacheForm.clearProportion" :placeholder="$t('Cache.enterCleaningRatioPercent')" style="width: 100%"
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
        callBack(this.$t('Cache.KeepMinimumUnitsConsistent'))
      } else if (this.cacheForm.minSizeUnit === this.cacheForm.maxSizeUnit && new Number(value) >= new Number(this.cacheForm.maxSize)) {
        callBack(this.$t('Cache.MinimumCacheValue'))
      } else {
        callBack()
      }
    }
    const maxSizeValidator = (rule, value, callBack) => {
      if (value && this.cacheForm.minSize && this.cacheForm.minSizeUnit !== this.cacheForm.maxSizeUnit) {
        callBack(this.$t('Cache.KeepMaximumUnitsConsistent'))
      } else if (this.cacheForm.minSizeUnit === this.cacheForm.maxSizeUnit && new Number(value) <= new Number(this.cacheForm.minSize)) {
        callBack(this.$t('Cache.MaximumCacheValue'))
      } else {
        callBack()
      }
    }
    const directoryPathValidator = (rule, value, callBack) => {
      if (!value) {
        callBack(this.$t('Cache.selectCacheDirectory'))
      } else {
        callBack()
      }
    }
    const clearProportionValidator = (rule, value, callBack) => {
      if (!value) {
        callBack(this.$t('Cache.enterCleaningRatio'))
      } else {
        callBack()
      }
    }
    const clearConditionValidator = (rule, value, callBack) => {
      if (!value) {
        callBack(this.$t('Cache.enterCleaningConditions'))
      } else {
        callBack()
      }
    }
    const sizeValidator = (rule, value, callback) => {
      if (value) {
        if(value.length < 1 || value.length > 10) {
          callback(new Error(this.$t('Cache.CacheSizeLengthLimit')))
        } else {
          callback()
        }
      } else if (!value) {
        callback(new Error(this.$t('Cache.enterCacheCapacity')))
      } else {
        callback()
      }
    }
    return {
      cacheRules: {
        directoryPath: [
          { required: true, trigger: ["blur", "change"], validator: directoryPathValidator },
        ],
        size: [
          { required: true, trigger: "blur", validator: sizeValidator },
        ],
        clearCondition: [
          { required: true, trigger: ['blur'], validator: clearConditionValidator },
        ],
        clearProportion: [
          { required: true, trigger: ['blur'], validator: clearProportionValidator },
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
        message = this.$t('Cache.OperationSuccessful')
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
        this.alertMsg('success',this.$t('Cache.ClearedCacheDirectorySuccess'))
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
          this.cacheDirectoryUseProportion = ( useSize / size * 100).toFixed(2)
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
            this.alertMsg('success',this.$t('Cache.cachePolicySetSuccess'))
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
