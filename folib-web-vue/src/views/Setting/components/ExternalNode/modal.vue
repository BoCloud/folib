<template>
  <div>
    <a-modal v-model="showExternalNode" :title="handlerExternalNodeType === 1 ? $t('ExternalNode.AddExternalNode') : $t('ExternalNode.ModifyExternalNode')" :maskClosable="false"
      :cancelText="$t('ExternalNode.Cancel')" :okText="$t('ExternalNode.Confirm')" @cancel="externalNodeHandlerCancel()" @ok="externalNodeHandlerConfirm()" centered>
      <a-form-model layout="horizontal" ref="externalNodeForm" :model="externalNodeForm" :rules="externalNodeRules"
        :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('ExternalNode.ExternalNodeName')" :colon="false" prop="nodeName">
              <a-input :placeholder="$t('ExternalNode.EnterExternalNodeName')" :disabled="handlerExternalNodeType === 2" v-model="externalNodeForm.nodeName" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('ExternalNode.ArtifactRepositoryType')" :colon="false" prop="type">
              <a-select v-model="externalNodeForm.type" :placeholder="$t('ExternalNode.selectArtifactType')" show-search optionFilterProp="label">
                <a-select-option v-for="(item, index) in artifactoryList" :label="item.label" :key="index"
                  :value="item.value">
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('ExternalNode.ExternalNodeAddress')" :colon="false" prop="address">
              <a-input :placeholder="$t('ExternalNode.EnterExternalNodeAddress')" v-model="externalNodeForm.address" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('ExternalNode.Username')" :colon="false" prop="username">
              <a-input :placeholder="$t('ExternalNode.EnterYourUsername')" v-model="externalNodeForm.username" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" :label="$t('ExternalNode.Password')" :colon="false" prop="password">
              <a-input-password :placeholder="$t('ExternalNode.EnterThePassword')" v-model="externalNodeForm.password" />
            </a-form-model-item>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
  </div>
</template>
<script>
import { saveExternalNode, updateExternalNode } from "@/api/externalNode"
import { encrypt } from "@/utils/jsencrypt"
export default {
  props: {
    modelVisible: {
      type: Boolean,
      default: false,
    },
    handlerExternalNodeType: {
      type: Number,
      default: false,
    },
    externalNode: {
      type: Object,
      default: undefined,
    },
  },
  components: {

  },
  data() {
    const acceptUrlValidator = (rule, value, callBack) => {
      let url = /^(?:http(s)?:\/\/)?[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/[\]@!\$&'\*\+,;=.]+$/;
      if (value) {
        if (!url.test(value)) {
          callBack(this.$t('ExternalNode.EnterValidURL'))
        } else if (value.length < 1 || value.length > 255) {
          callBack(this.$t('ExternalNode.addressLengthLimit'))
        } else {
          callBack()
        }
      } else {
        callBack(this.$t('ExternalNode.EnterExternalNodeAddress'))
      }
    }
    const nodeNameValidator = (rule, value, callBack) => {
      if (!value) {
        callBack(this.$t('ExternalNode.EnterExternalNodeName'))
      } else {
        callBack()
      }
    }
    const typeValidator = (rule, value, callBack) => {
      if (!value) {
        callBack(this.$t('ExternalNode.selectArtifactType'))
      } else {
        callBack()
      }
    }
    const usernameValidator = (rule, value, callBack) => {
      if (!value) {
        callBack(this.$t('ExternalNode.EnterYourUsername'))
      } else {
        callBack()
      }
    }
    const passwordValidator = (rule, value, callBack) => {
      if (!value) {
        callBack(this.$t('ExternalNode.EnterThePassword'))
      } else {
        callBack()
      }
    }
    return {
      artifactoryList: [{ label: "JFrog", value: "JFrog" }],
      externalNodeRules: {
        nodeName: [{ required: true, trigger: "blur", validator: nodeNameValidator }],
        type: [{ required: true, trigger: "blur", validator: typeValidator }],
        address: [
          { required: true, trigger: ['blur'], validator: acceptUrlValidator }
        ],
        username: [
          { required: true, trigger: "blur", validator: usernameValidator },
        ],
        password: [{ required: true, trigger: "blur", validator: passwordValidator }],
      },
      externalNodeForm: {
        id: undefined,
        nodeName: undefined,
        type: "JFrog",
        address: undefined,
        username: undefined,
        password: undefined,
      },
      showExternalNode: false,
    };
  },
  created() {
    if (this.modelVisible) {
      this.resetExternalNodeForm()
      this.showExternalNode = this.modelVisible
      if (this.externalNode) {
        this.externalNodeForm = Object.assign({}, this.externalNode)
        if (this.handlerExternalNodeType === 2) {
          this.externalNodeRules.password[0].required = false
        }
      }
    }
  },
  watch: {

  },
  mounted() { },
  methods: {
    successMsg(message) {
      if (!message) {
        message = this.$t('ExternalNode.OperateSuccess')
      }
      this.$notification["success"]({
        message: message,
      })
    },
    resetExternalNodeForm() {
      this.externalNodeForm = {
        nodeName: undefined,
        type: "JFrog",
        address: undefined,
        username: undefined,
        password: undefined,
      }
      if (this.$refs.externalNodeForm) {
        this.$refs.externalNodeForm.resetFields()
      }
    },
    externalNodeHandlerCancel() {
      this.resetExternalNodeForm()
      this.$emit("externalNodeHandlerCancel")
    },
    externalNodeHandlerConfirm() {
      this.$refs.externalNodeForm.validate((valid) => {
        if (valid) {
          let data = Object.assign({}, this.externalNodeForm)
          if (data.password) {
            data.password = encrypt(data.password)
          }
          if (this.handlerExternalNodeType === 1) {
            saveExternalNode(data).then((res) => {
              this.successMsg(this.$t('ExternalNode.ExternalNodeAddSuccess'))
              this.resetExternalNodeForm()
              this.$emit("externalNodeReflesh")
            }).catch((err) => {
              this.$notification["error"]({
                message: err.response.data.error,
              })
            }).finally(() => { })
          } else {
            updateExternalNode(data).then((res) => {
              this.successMsg(this.$t('ExternalNode.ExternalNodeModifySuccess'))
              this.resetExternalNodeForm()
              this.$emit("externalNodeReflesh")
            }).catch((err) => {
              this.$notification["error"]({
                message: err.response.data.error,
              })
            }).finally(() => { })
          }
        } else {
          return false
        }
      })
    },
  },
};
</script>
