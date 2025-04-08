<template>
  <div>
    <h6>{{ $t('SystemImportExport.ExportSystem') }}</h6>
    <a-form-model layout="horizontal" ref="systemExportRef" :model="systemExportForm"
      :rules="systemExportRules" :hideRequiredMark="false">
      <a-form-model-item class="mb-10" :label="$t('SystemImportExport.ExportPath')" :colon="false" prop="path">
        <a-tree-select v-model="systemExportForm.path" class="system-import-export-form-common"
          :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }" :tree-data="directoryPaths"
          :placeholder="$t('SystemImportExport.ChooseExportPath')" :load-data="onLoadData" />
      </a-form-model-item>
      <a-form-model-item class="mb-10" :colon="false" prop="zipArchive">
        <a-checkbox v-model="systemExportForm.zipArchive">
          {{ $t('SystemImportExport.ZipArchive') }}
        </a-checkbox>
      </a-form-model-item>
      <a-form-model-item :wrapper-col="{ span: 14, offset: 0 }">
        <a-button type="primary" @click="systemExportFormSubmit">
          {{ $t('SystemImportExport.Export') }}
        </a-button>
      </a-form-model-item>
    </a-form-model>
    <h6>{{ $t('SystemImportExport.ImportSystem') }}</h6>
    <a-tag color="orange">
      <a-icon type="exclamation" /> {{ $t('SystemImportExport.ImportTip') }}
    </a-tag>
    <a-form-model layout="horizontal" ref="systemImportRef" :model="systemImportForm"
      :rules="systemImportRules" :hideRequiredMark="false">
      <a-form-model-item class="mb-10" :label="$t('SystemImportExport.ImportPath')" :colon="false" prop="path">
        <a-tree-select v-model="systemImportForm.path" class="system-import-export-form-common"
          :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }" :tree-data="directoryPaths"
          :placeholder="$t('SystemImportExport.ChooseImportPath')" :load-data="onLoadData" />
      </a-form-model-item>
      <a-form-model-item :wrapper-col="{ span: 14, offset: 0 }">
        <a-popconfirm :title="$t('SystemImportExport.SureImport')" okType="danger" :ok-text="$t('SystemImportExport.BeSure')" :cancel-text="$t('SystemImportExport.Cancel')"
          @confirm="systemImportFormSubmit">
          <a-button type="primary">
          {{ $t('SystemImportExport.Import') }}
        </a-button>
        </a-popconfirm>
      </a-form-model-item>
    </a-form-model>
  </div>
</template>
<script>
import {
  folderList
} from "@/api/advanced"
import {
  systemExport,
  systemImport
} from "@/api/systemImportExport"
export default {
  props: {

  },
  data() {
    const checkExportPath = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('SystemImportExport.ChooseExportPath')))
      } else {
        callback()
      }
    }
    const checkImportPath = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('SystemImportExport.ChooseImportPath')))
      } else {
        callback()
      }
    }
    return {
      systemExportForm: {
        path: null,
        zipArchive: false,
      },
      systemImportForm: {
        path: null,
      },
      directoryPaths: [],
      systemExportRules: {
        path: [
          { required: true, trigger: ['blur', 'change'], validator: checkExportPath },
        ],
      },
      systemImportRules: {
        path: [
          { required: true, trigger: ['blur', 'change'], validator: checkImportPath },
        ],
      },
    }
  },
  components: {
  },
  computed: {
  
  },
  created() {
    this.initData()
  },
  mounted() { },
  methods: {
    successMsg(message) {
      if (!message) {
        message = this.$t('SystemImportExport.OperationSuccessful');
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    initData() {
      this.folderList()
    },
    folderList() {
      folderList({ directoryPath: "/", includesSuffix: '.zip' }).then(res => {
        this.directoryPaths = []
        if (res) {
          res.forEach(item => {
            this.directoryPaths.push({
              key: item.fullPath,
              title: item.name,
              value: item.fullPath,
              isLeaf: item.file
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
        folderList({ directoryPath: treeNode.dataRef.value, includesSuffix: '.zip' }).then((res) => {
          if (res) {
            res.forEach((item) => {
              let obj = {}
              obj = {
                key: item.fullPath,
                title: item.name,
                value: item.fullPath,
                isLeaf: item.file
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
    systemExportFormSubmit() {
      this.$refs.systemExportRef.validate(valid => {
        if (valid) {
          let dataForm = Object.assign({}, this.systemExportForm)
          systemExport(dataForm).then(res => {
            this.successMsg(this.$t('SystemImportExport.OperationSuccessful'))
          }).catch((err) => {
            let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
            if (msg && msg.length > 0) {
              this.$notification.error({
                message: msg,
                description: ""
              })
            }
          })
        }
      })
    },
    systemExportResetForm() {
      this.systemExportForm = {
        path: null,
        zipArchive: false,
      }
      if (this.$refs.systemExportRef) {
        this.$refs.systemExportRef.resetFields()
      }
    },
    systemImportFormSubmit() {
      this.$refs.systemImportRef.validate(valid => {
        if (valid) {
          let dataForm = Object.assign({}, this.systemImportForm)
          systemImport(dataForm).then(res => {
            this.successMsg(this.$t('SystemImportExport.OperationSuccessful'))
          }).catch((err) => {
            let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
            if (msg && msg.length > 0) {
              this.$notification.error({
                message: msg,
                description: ""
              })
            }
          })
        }
      })
    },
    systemImportResetForm() {
      this.systemImportForm = {
        path: null,
      }
      if (this.$refs.systemImportRef) {
        this.$refs.systemImportRef.resetFields()
      }
    },
  },
};
</script>

<style lang="scss" scoped>
.system-import-export-form-common {
  width: 450px
}
</style>
