<template>
  <div>
    <a-modal v-model="showPackageName" :title="$t('Package.CreatePackage')" :maskClosable="false"
      :cancelText="$t('Package.Cancel')" :okText="$t('Package.Confirm')" @cancel="packageNameHandlerCancel()" @ok="packageNameHandlerConfirm()" centered>
      <a-form-model layout="horizontal" ref="packageNameForm" :model="packageNameForm" :rules="packageNameRules"
        :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Package.PackageName')" :colon="false" prop="packageName">
              <a-input :placeholder="$t('Package.PackageNamePlaceholder')" v-model="packageNameForm.packageName" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Package.ConditionType')" :colon="false" prop="condition">
              <a-select v-model="packageNameForm.condition" :placeholder="$t('Package.ConditionTypePlaceholder')" show-search allowClear optionFilterProp="label">
                <a-select-option v-for="(item, index) in conditionList" :label="item.label" :key="index"
                  :value="item.value">
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('Package.Version')" :colon="false" prop="version">
              <a-input :placeholder="$t('Package.VersionPlaceholder')" v-model="packageNameForm.version" />
            </a-form-model-item>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
  </div>
</template>
<script>
import {
  savePackageNameBlock,
} from '@/api/packageNameBlock'

export default {
  props: {
    modelVisible: {
      type: Boolean,
      default: false,
    },
  },
  components: {

  },
  data() {
    const acceptConditionValidator = (rule, value, callBack) => {
      if (!value && this.packageNameForm.version) {
        callBack(this.$t('Package.ConditionTypePlaceholder'))
      } else {
        callBack()
      }
    }
    const acceptVersionValidator = (rule, value, callBack) => {
      if (!value && this.packageNameForm.condition) {
        callBack(this.$t('Package.VersionPlaceholder'))
      } else {
        callBack()
      }
    }
    return {
      conditionList: [
        { label: "=", value: "=" },
        { label: "<", value: "<" },
        { label: "<=", value: "<=" },
      ],
      packageNameRules: {
        packageName: [
          { required: true, message: this.$t('Package.PackageNamePlaceholder'), trigger: "blur" },
          { min: 1, max: 300, message: this.$t('Package.PackageNameLength'), trigger: 'blur' },
        ],
        condition: [
          { required: false, trigger: ['blur'], validator: acceptConditionValidator },
        ],
        version: [
          { required: false, trigger: ['blur'], validator: acceptVersionValidator },
          { min: 1, max: 100, message: this.$t('Package.VersionLength'), trigger: 'blur' }
        ],
      },
      packageNameForm: {
        packageName: undefined,
        condition: undefined,
        version: undefined,
      },
      showPackageName: false,
    };
  },
  created() {
    if (this.modelVisible) {
      this.resetPackageNameForm()
      this.showPackageName = this.modelVisible
    }
  },
  watch: {

  },
  mounted() { },
  methods: {
    successMsg(message) {
      if (!message) {
        message = this.$t("Package.OperateSuccess")
      }
      this.$notification["success"]({
        message: message,
      })
    },
    resetPackageNameForm() {
      this.packageNameForm =  {
        packageName: undefined,
        condition: undefined,
        version: undefined,
      }
      if (this.$refs.packageNameForm) {
        this.$refs.packageNameForm.resetFields()
      }
    },
    packageNameHandlerCancel() {
      this.resetPackageNameForm()
      this.$emit("packageNameHandlerCancel")
    },
    packageNameHandlerConfirm() {
      this.$refs.packageNameForm.validate((valid) => {
        if (valid) {
          let data = {
            packageName: this.packageNameForm.packageName
          }
          if (this.packageNameForm.condition && this.packageNameForm.version) {
            if (this.packageNameForm.condition === '<') {
              data.conditionValue =  'range'
              data.version = '(*,' + this.packageNameForm.version + ')'
            } else if (this.packageNameForm.condition === '<=') {
              data.conditionValue =  'range'
              data.version = '(*,' + this.packageNameForm.version + ']'
            } else {
              data.conditionValue =  'eq'
              data.version = this.packageNameForm.version
            }
          }
          savePackageNameBlock(data).then(res => {
            this.successMsg(this.$t('Package.CreatePackage') + ' ' + this.packageNameForm.packageName + this.$t('Package.Success'))
          }).catch(err => {
            this.$notification['error']({
              message: err.response.data.error,
              description: ''
            })
          }).finally(() => {
            this.packageNameHandlerCancel()
            this.$emit("packageNameRefresh")
          })
        } else {
          return false
        }
      })
    },
  },
}
</script>