<template>
  <div>
    <a-modal v-model="showPackageName" title="新增包名" :maskClosable="false"
      cancelText="取消" okText="确定" @cancel="packageNameHandlerCancel()" @ok="packageNameHandlerConfirm()" centered>
      <a-form-model layout="horizontal" ref="packageNameForm" :model="packageNameForm" :rules="packageNameRules"
        :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" label="包名称" :colon="false" prop="packageName">
              <a-input placeholder="请输入包名称" v-model="packageNameForm.packageName" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" label="条件类型" :colon="false" prop="condition">
              <a-select v-model="packageNameForm.condition" placeholder="请选择条件类型" show-search allowClear optionFilterProp="label">
                <a-select-option v-for="(item, index) in conditionList" :label="item.label" :key="index"
                  :value="item.value">
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" label="版本号" :colon="false" prop="version">
              <a-input placeholder="请输入版本号" v-model="packageNameForm.version" />
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
        callBack("请选择条件")
      } else {
        callBack()
      }
    }
    const acceptVersionValidator = (rule, value, callBack) => {
      if (!value && this.packageNameForm.condition) {
        callBack("请输入版本号")
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
          { required: true, message: "请输入包名称", trigger: "blur" },
          { min: 1, max: 300, message: '包名称长度在1到300个字符', trigger: 'blur' },
        ],
        condition: [
          { required: false, trigger: ['blur'], validator: acceptConditionValidator },
        ],
        version: [
          { required: false, trigger: ['blur'], validator: acceptVersionValidator },
          { min: 1, max: 100, message: '版本号长度在1到100个字符', trigger: 'blur' }
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
        message = "操作成功"
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
            this.successMsg('添加包名 ' + this.packageNameForm.packageName + ' 成功')
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