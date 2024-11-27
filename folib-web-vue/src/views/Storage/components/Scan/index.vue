<template>
  <div>
    <a-form-model
      layout="horizontal"
      ref="scanForm"
      :model="scanForm"
      :rules="scanRules"
      :hideRequiredMark="true"
    >
      <a-row :gutter="[24]">
        <a-col :span="24">
          <a-card :bordered="false" class="header-solid">
            <template #title>
              <h6>{{ $t('Scan.ArtifactsScan') }}</h6>
              <p v-if="scanForm.onScan === true">{{ $t('Scan.ArtifactsScanDesc') }}</p>
              <p v-else>{{ $t('Scan.ArtifactsScanDesc') }}</p>
            </template>
            <a-radio-group v-model="scanForm.onScan">
              <a-radio :value="true">
                {{ $t('Scan.On') }}
              </a-radio>
              <a-radio :value="false">
                {{ $t('Scan.Off') }}
              </a-radio>
            </a-radio-group>
          </a-card>
          <a-card :bordered="false" class="header-solid"  v-if="this.foeyesEnable && this.folibRepository.type === 'hosted' && this.bomLayoutList.includes(this.folibRepository.layout)">
            <template #title>
              <h6>{{ $t('Scan.BOMScan') }}</h6>
              <p v-if="scanForm.bomOnScan === 1">{{ $t('Scan.BOMScanDesc') }}</p>
              <p v-else>{{ $t('Scan.BOMScanDesc') }}</p>
            </template>
            <a-radio-group v-model="scanForm.bomOnScan">
              <a-radio :value="true">
                {{ $t('Scan.On') }}
              </a-radio>
              <a-radio :value="false">
                {{ $t('Scan.Off') }}
              </a-radio>
            </a-radio-group>
          </a-card>
          <a-form-model-item :wrapper-col="{ span: 14, offset: 4 }">
            <a-button type="primary" @click="scanFormSubmit" v-if="isShow">
              {{ $t('Scan.Save') }}
            </a-button>
            <a-button class="ml-10" @click="scanResetForm" v-if="isShow">
              {{ $t('Scan.Cancel') }}
            </a-button>
          </a-form-model-item>
        </a-col>
      </a-row>
    </a-form-model>
  </div>
</template>
<script>
import {
  scannerRules,
  insertOrUpdateRules,
} from "@/api/folib"
import {
  getCacheConfig
} from "@/api/foEyes"
export default {
  inject: ['doDrawerStatus'],
  props: {
		folibRepository: {
			type: Object,
			default: {},
    },
    settingVisible: {
			type: Boolean,
			default: false,
		},
    isShow:{
        type: Boolean,
        default: true,
    },
	},
  data() {
    return {
      foeyesEnable: false,
      scanForm: {
        onScan: false,
        bomOnScan: false,
		  },
      scanRules: {
       
      },
      bomLayoutList: [
        'Raw',
        'Docker'
      ],
    }
  },
  components: {

  },
  computed: {
  
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
  mounted() {},
  methods: {
    successMsg(message) {
      if (!message) {
        message = this.$t('Scan.OperationSuccessful');
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    initData () {
      this.getFoEyesEnable()
      this.getScannerRules()
    },
    getScannerRules () {
      scannerRules(
        this.folibRepository.storageId + '-' + this.folibRepository.id
      ).then(res => {
        if (res.rel) {
          this.scanForm = res.data
        }
      })
    },
    getFoEyesEnable () {
      const cacheConfig = getCacheConfig()
      if (cacheConfig) {
        this.foeyesEnable = cacheConfig.enable
      }
    },
    scanFormSubmit() {
      this.$refs.scanForm.validate(valid => {
        if (valid) {
          let data = {
            id: this.folibRepository.storageId + "-" + this.folibRepository.id,
            storage: this.folibRepository.storageId,
            repository: this.folibRepository.id,
            layout: this.folibRepository.layout,
            onScan: this.scanForm.onScan,
            bomOnScan: this.scanForm.bomOnScan,
          }
          insertOrUpdateRules(data).then(res => {
            this.successMsg(this.$t('Scan.OperationSuccessful'))
            setTimeout(() => {
              this.scanResetForm()
                this.callParent(true,'process')
            }, 100);
          }).catch((err) => {
              let error = err.response.data.message?err.response.data.message:this.$t('Storage.UnknownError')
              this.$notification["error"]({
                message: error,
              })
            this.callParent(false,'error')
          })
        }
      })
    },
    scanResetForm() {
      this.$refs.scanForm.resetFields()
      this.$emit('settingDrawerClose')
    },
    callParent(isClose,status) {
        if(this.doDrawerStatus){
            this.doDrawerStatus(false,status)
        }
    }
  },
};
</script>

<style lang="scss" scoped>

</style>
