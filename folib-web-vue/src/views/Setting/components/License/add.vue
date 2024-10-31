<template>
  <div>
    <a-modal v-model="showLicenseModal" :title="blackWhiteType==1?$t('Setting.AddLicenseWhitelist'):$t('Setting.AddLicenseBlacklist')" :maskClosable="false"
      :cancelText="$t('Package.Cancel')" :okText="$t('Package.Confirm')" @cancel="licenseModalCancel()" @ok="licenseHandlerConfirm()" centered>
      <a-form-model layout="horizontal" ref="licenseForm" :model="licenseForm" :rules="licenseRules"
        :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :colon="false" prop="licenseId">
              <a-select v-model="licenseForm.licenseId" :placeholder="$t('Setting.PleaseSelectLicense')" show-search allowClear optionFilterProp="label">
                <a-select-option v-for="(item, index) in licenseList" :label="item.licenseId" :key="index"
                  :value="item.licenseId">
                  {{ item.licenseId }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
  </div>
</template>
<script>
import { getLicenses, blackWhite } from "@/api/licenses.js"
import { message } from "ant-design-vue";

export default {
  props: {
    modelVisible: {
      type: Boolean,
      default: false,
    },
    blackWhiteType: {
      type: Number,
      default: 1,
    },
  },
  components: {

  },
  data() {
    const checkLicense = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Setting.PleaseSelectLicense')))
      } else {
				callback()
			}
    }
    return {
      licenseRules: {
        licenseId: [
          { required: true, trigger: ['blur'], validator: checkLicense},
        ],
      },
      licenseForm: {
        licenseId: undefined,
      },
      showLicenseModal: false,
      licenseList: [],
    };
  },
  created() {
    if (this.modelVisible) {
      this.resetLicenseForm()
      this.getLicenses()
      this.showLicenseModal = this.modelVisible
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
    getLicenses() {
      getLicenses({blackWhiteType: 0}).then((res) => {
        this.licenseList = res
      }).finally(() => {
      })
    },
    resetLicenseForm() {
      this.licenseForm =  {
        licenseId: undefined,
      }
      if (this.$refs.licenseForm) {
        this.$refs.licenseForm.resetFields()
      }
    },
    licenseModalCancel() {
      this.resetLicenseForm()
      this.$emit("licenseModalCancel")
    },
    licenseHandlerConfirm() {
      this.$refs.licenseForm.validate((valid) => {
        if (valid) {
          blackWhite({licenseId: this.licenseForm.licenseId, blackWhiteType: this.blackWhiteType}).then((res) => {
            this.successMsg(this.$t('Package.OperateSuccess'))
            this.licenseModalCancel()
            this.$emit("licenseRefresh")
          }).finally(() => {
          })
        }
      })
    },
  },
}
</script>