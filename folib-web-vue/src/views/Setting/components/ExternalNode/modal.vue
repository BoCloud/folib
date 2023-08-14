<template>
  <div>
    <a-modal v-model="showExternalNode" :title="handlerExternalNodeType === 1 ? '新增外部节点' : '修改外部节点'" :maskClosable="false"
      cancelText="取消" okText="确定" @cancel="externalNodeHandlerCancel()" @ok="externalNodeHandlerConfirm()" centered>
      <a-form-model layout="horizontal" ref="externalNodeForm" :model="externalNodeForm" :rules="externalNodeRules"
        :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" label="外部节点名称" :colon="false" prop="nodeName">
              <a-input placeholder="请输入外部节点名称" :disabled="handlerExternalNodeType === 2" v-model="externalNodeForm.nodeName" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" label="制品库类型" :colon="false" prop="type">
              <a-select v-model="externalNodeForm.type" placeholder="请选择制品库类型" show-search optionFilterProp="label">
                <a-select-option v-for="(item, index) in artifactoryList" :label="item.label" :key="index"
                  :value="item.value">
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" label="外部节点地址" :colon="false" prop="address">
              <a-input placeholder="请输入外部节点地址" v-model="externalNodeForm.address" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" label="用户名" :colon="false" prop="username">
              <a-input placeholder="请输入用户名" v-model="externalNodeForm.username" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10" label="密码" :colon="false" prop="password">
              <a-input-password placeholder="请输入密码" v-model="externalNodeForm.password" />
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
          callBack("请输入正确的URL")
        } else {
          callBack()
        }
      } else {
        callBack("请输入外部节点地址")
      }
    }
    return {
      artifactoryList: [{ label: "JFrog", value: "JFrog" }],
      externalNodeRules: {
        nodeName: [{ required: true, message: "请输入外部节点名称", trigger: "blur" }],
        type: [{ required: true, message: "请选择制品库类型", trigger: "blur" }],
        address: [
          { required: true, trigger: ['blur'], validator: acceptUrlValidator },
          {
            min: 1,
            max: 255,
            message: "长度在 1 到 255 个字符",
            trigger: "blur",
          },
        ],
        username: [
          { required: true, message: "请输入用户名", trigger: "blur" },
        ],
        password: [{ required: true, message: "请输入密码", trigger: "blur" }],
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
        message = "操作成功"
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
              this.successMsg("新增外部节点成功")
              this.resetExternalNodeForm()
              this.$emit("externalNodeReflesh")
            }).catch((err) => {
              this.$notification["error"]({
                message: err.response.data.error,
              })
            }).finally(() => { })
          } else {
            updateExternalNode(data).then((res) => {
              this.successMsg("修改外部节点成功")
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