<template>
  <div>
    <a-modal v-model="showLicenseModal" :title="blackWhiteType==1?$t('Setting.AddLicenseWhitelist'):$t('Setting.AddLicenseBlacklist')" :maskClosable="false"
      :cancelText="$t('Package.Cancel')" :okText="$t('Package.Confirm')" @cancel="licenseModalCancel()" @ok="licenseHandlerConfirm()" centered>
      <a-form-model layout="horizontal" ref="licenseForm" :model="licenseForm" :rules="licenseRules"
        :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10" :colon="false" prop="identifier"  label="License">
              <a-select v-model="licenseForm.identifier" :placeholder="$t('Setting.PleaseSelectLicense')" :disabled="isEdit" show-search allowClear optionFilterProp="label">
                <a-select-option v-for="(item, index) in licenseList" :label="item.licenseId" :key="index"
                  :value="item.licenseId">
                  {{ item.licenseId }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
            <a-form-model-item class="mb-10" :colon="false" :label="$t('Setting.validFrom')" >
              <a-date-picker  :default-value="monthLater" v-model="licenseForm.validFrom" style="width: 100%" />
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
import moment from "moment/moment";
import {addAllowlistDenylistBlock, updateAllowlistDenylistBlock} from "@/api/AllowlistDenylistBlock";

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
    recordData:{
      type: Object,
      default: undefined,
    }
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
        identifier: [
          { required: true, trigger: ['blur'], validator: checkLicense},
        ],
      },
      licenseForm: {
        identifier: undefined,
        validFrom: undefined,
        id: undefined,
      },
      showLicenseModal: false,
      licenseList: [],
      monthLater: undefined,//this.calculateOneMonthLater(),
      isEdit: false,
    };
  },
  created() {
    if (this.modelVisible) {
      this.resetLicenseForm()
      this.getLicenses()
      this.showLicenseModal = this.modelVisible
    }
    if(this.recordData){
      this.licenseForm.identifier = this.recordData.identifier
      this.licenseForm.validFrom = this.recordData.validFrom
      this.licenseForm.id = this.recordData.id
      this.isEdit = true
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
        identifier: undefined,
        validFrom: undefined,
        id: undefined,
      }
      this.isEdit = false;
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
          // blackWhite({licenseId: this.licenseForm.licenseId, blackWhiteType: this.blackWhiteType}).then((res) => {
          //   this.successMsg(this.$t('Package.OperateSuccess'))
          //   this.licenseModalCancel()
          //   this.$emit("licenseRefresh")
          // }).finally(() => {
          // })
          let data = {
            id:this.licenseForm.id,
            type: undefined,
            category: 'LICENSE',
            identifier: this.licenseForm.identifier,
            validFrom: this.licenseForm.validFrom ? moment(this.licenseForm.validFrom).format('YYYY-MM-DD HH:mm:ss') : undefined,
          }
          if (this.isEdit){
            this.updateLicense(data)
          }else {
            this.addLicense(data)
          }

        }
      })
    },


    updateLicense(data) {
      if (this.blackWhiteType === 1) {
        data.type='WHITES';
        updateAllowlistDenylistBlock(data).then(res=>{
          this.successMsg(this.$t('Package.OperateSuccess'))
          this.licenseModalCancel()
          this.$emit("licenseRefresh")
        }).catch(err=>{
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        }).finally();
      } else if (this.blackWhiteType === 2) {
        data.type='BLACKLIST';
        updateAllowlistDenylistBlock(data).then(res=>{
          this.successMsg(this.$t('Package.OperateSuccess'))
          this.licenseModalCancel()
          this.$emit("licenseRefresh")
        }).catch(err=>{
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        }).finally();
      }
    },

    addLicense(data) {
      if (this.blackWhiteType === 1) {
        data.type='WHITES';
        addAllowlistDenylistBlock(data).then(res=>{
          this.successMsg(this.$t('Package.OperateSuccess'))
          this.licenseModalCancel()
          this.$emit("licenseRefresh")
        }).catch(err=>{
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        }).finally()
      } else if (this.blackWhiteType === 2) {
        data.type='BLACKLIST';
        addAllowlistDenylistBlock(data).then(res=>{
          this.successMsg(this.$t('Package.OperateSuccess'))
          this.licenseModalCancel()
          this.$emit("licenseRefresh")
        }).catch(err=>{
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        }).finally()
      }
    },

    calculateOneMonthLater() {
      const currentDate = new Date();
      const oneMonthLater = new Date(currentDate);
      oneMonthLater.setMonth(currentDate.getMonth() + 6);
      return moment(oneMonthLater).startOf('day');
    },
  },
}
</script>