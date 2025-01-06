<template>
  <div>
    <a-form-model ref="formRef" :model="jfrogMigrationForm" :rules="jfrogMigrationRules" >
      <a-row :gutter="24">
        <a-col :span="7">
          <a-form-model-item :label="$t('Setting.JfrogUrl')" :colon="false" prop="url">
            <a-input v-model="jfrogMigrationForm.url" />
          </a-form-model-item>
        </a-col>
        <a-col :span="7">
          <a-form-model-item :label="$t('Setting.JfrogUsername')" :colon="false" prop="username">
            <a-input v-model="jfrogMigrationForm.username" />
          </a-form-model-item>
        </a-col>
        <a-col :span="7">
          <a-form-model-item :label="$t('Setting.JfrogPassword')" :colon="false" prop="password">
            <a-input type="password" v-model="jfrogMigrationForm.password" />
          </a-form-model-item>
        </a-col>
      </a-row>
      
      <a-row :gutter="24">
        <a-col :span="7">
          <a-form-model-item :colon="false" prop="storageId">
            <template slot="label">
              {{ $t('Setting.StorageSpace') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <p class="mb-0">{{ $t('Setting.StorageProvide') }}</p>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>

            <a-select v-model="jfrogMigrationForm.storageId" mode="combobox"
              :placeholder="$t('Setting.selectTheStorageSpace')" @change="handleStorageChange" clearable>
              <a-select-option v-for="storage in storages" :key="storage.id">
                {{ storage.id }}
              </a-select-option>
            </a-select>
          </a-form-model-item>
        </a-col>
        <a-col :span="7">
          <a-form-model-item :label="$t('Setting.StorageProvider')" :colon="false" prop="storageProvider">
            <!-- <a-input v-model="jfrogMigrationForm.storageProvider" /> -->
            <a-select v-model="jfrogMigrationForm.storageProvider" :placeholder="$t('Setting.selectTheStorageProvider')"
              :disabled="isExistingStorage">
              <a-select-option value="local">local</a-select-option>
              <a-select-option value="s3">s3</a-select-option>
            </a-select>
          </a-form-model-item>
        </a-col>
        <a-col :span="7">
          <a-form-model-item :colon="false" prop="basedir">
            <template slot="label">
              {{ jfrogMigrationForm.storageProvider === 's3' ? $t('Setting.BucketName') : $t('Setting.Basedir') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <p v-if="jfrogMigrationForm.storageProvider != 's3'" class="mb-0">{{
                    $t('Setting.StorageProvideLocalInfo') }}</p>
                  <p v-if="jfrogMigrationForm.storageProvider === 's3'" class="mb-0">{{
                    $t('Setting.StorageProvideS3Info') }}</p>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-input v-model="jfrogMigrationForm.basedir" :disabled="isExistingStorage" />
          </a-form-model-item>
        </a-col>
      </a-row>
      <a-row :gutter="24">
        <a-col :span="7">
          <a-form-model-item :label="$t('Setting.ArtifactType')" :colon="false" prop="artifactType">
            <a-select v-model="jfrogMigrationForm.artifactType" :placeholder="$t('Setting.PleaseSelectArtifactType')">
                <a-select-option value="1">{{ $t('Setting.BaseOnJfrog') }}</a-select-option>
                <a-select-option value="2">{{ $t('Setting.BaseOnBrowse') }}</a-select-option>
            </a-select>
          </a-form-model-item>
        </a-col>
        <a-col :span="14">
          <a-form-model-item 
          :label="$t('Setting.MigrateContent')" 
          prop="contents"
          :colon="false" 
        >
          <a-checkbox-group 
            v-model="jfrogMigrationForm.contents" 
            style="width: 100%;" 
            @change="handleContentChange"
          >
            <a-row :gutter="24">
              <a-col :span="6" v-for="content in contentOptions" :key="content.value">
                <a-checkbox :value="content.value">
                  {{ $t(content.i18nKey) }}
                </a-checkbox>
              </a-col>
            </a-row>
          </a-checkbox-group>
          </a-form-model-item>
        </a-col>
      </a-row>
      <a-row>
        <a-col :span="18">
          <a-button type="danger" :loading="loading" @click="handleSubmit" >
            {{ $t('Setting.BeSure') }}
          </a-button>
          <a-button class="ml-10" @click="handleCancel">
            {{ $t('Setting.Cancel') }}
          </a-button>
        </a-col>
        <!-- <a-col :span="4">
         <a-button type="primary" class="ml-10" @click="showRestoreModal">
          <a-icon type="upload" />
          {{ $t('Setting.RestoreRepo') }}
        </a-button>
        </a-col> -->
      </a-row>
    </a-form-model>

    <a-modal
      :title="$t('Setting.RestoreRepo')"
      :visible="restoreModalVisible"
      @cancel="handleRestoreCancel"
      @ok="handleRestoreOk"
      :confirmLoading="restoreLoading"
    >
      <a-form-model>
        <p class="warning-text">{{ $t('Setting.RestoreWarning') }}</p>
        <a-row>
          <a-col :span="6">
            <a-form-model-item>
              <a-button type="download" @click="downloadTemplate">
            <a-icon type="download" />
            {{ $t('Setting.DownloadTemplate') }}
            </a-button>
          </a-form-model-item>
          </a-col>
          <a-col :span="6">
            <a-form-model-item>
              <a-upload
                :multiple="false"
                accept=".xls,.xlsx"
                :customRequest="customUploadRequest"
                :fileList="fileList"
                :remove="removeFile"
          >
            <a-button>
              <a-icon type="upload" />
              {{ $t('Setting.SelectFile') }}
            </a-button>
          </a-upload>
        </a-form-model-item>
        </a-col>
        
      </a-row>
      </a-form-model>
    </a-modal>

  </div>
</template>
<script>

import { getStorages, jfrogMigrate,importJfrog,downloadTemplate } from '@/api/folib'



export default {
  inject: ["reload"],
  data() {
    // 自定义校验规则
    const validateContents = (rule, value, callback) => {
      if (!value || value.length === 0) {
        callback(new Error(this.$t('Setting.PleaseSelectAtLeastOneContent')));
      } else {
        callback();
      }
    };

    return {
      jfrogMigrationForm: {
        url: undefined,
        username: undefined,
        password: undefined,
        storageId: undefined,
        storageProvider: undefined,
        basedir: "",
        artifactType:undefined,
        contents: [],
        previousCheckedValues:[],        
      },
      jfrogMigrationRules: {
        url: [{ required: true, trigger: 'blur', message: this.$t('Setting.PleaseEnterTheJfrogAddress') }],
        username: [{ required: true, trigger: 'blur', message: this.$t('Setting.PleaseEnterTheJfrogUsername') }],
        password: [{ required: true, trigger: 'blur', message: this.$t('Setting.PleaseEnterTheJfrogPassword') }],
        artifactType: [{ required: true, trigger: 'blur', message: this.$t('Setting.PleaseSelectArtifactType') }],
        contents: [
          { 
            required: true, 
            validator: validateContents, 
            trigger: 'change' 
          }
        ],
      },
      storages: [],
      isExistingStorage: false,
      loading:false,
      contentOptions: [
        { value: 'REPOSITORY', i18nKey: 'Setting.Repository' },
        { value: 'USER', i18nKey: 'Setting.User' },
        { value: 'GROUP', i18nKey: 'Setting.Group' },
        { value: 'PERMISSION', i18nKey: 'Setting.Permission' }
      ],
      restoreModalVisible: false,
      restoreLoading: false,
      fileList: [],
    }
  },

  mounted() {
    this.initData();
  },





  methods: {
    initData() {
      getStorages().then(res => {
        this.storages = res.storages;
      })
    },
    handleSubmit() {
      this.$refs.formRef.validate((valid) => {
        if (valid) {
          const data = { ...this.jfrogMigrationForm };
          if (data.storageProvider === 's3') {
            data.basedir = '/' + data.storageId + '/' + data.basedir;
          }
          this.loading = true;
          console.log('this.loading',this.loading);
          jfrogMigrate(data).then(res => {
            this.$message.success(this.$t('Setting.MigrateSuccess'))
          }).catch(err => {
            console.log('err',err);
            const errMsg=err.response?err.response.data.error:err.message;
            this.$notification['error']({
              message: errMsg,
              description: ''
            })
          }).finally(() => {
            this.loading = false;
            console.log('this.loading',this.loading);
          });
        } else {
          console.log('表单验证失败');
          return false;
        }
      })
    },
    handleCancel() {
      this.$refs.formRef.resetFields();
    },
    handleStorageChange(value) {
      const selectedStorage = this.storages.find(storage => storage.id === value);
      if (selectedStorage) {
        this.jfrogMigrationForm.storageProvider = selectedStorage.storageProvider || '';
        this.jfrogMigrationForm.basedir = selectedStorage.basedir || '';
        if (this.jfrogMigrationForm.storageProvider === 's3') {
          this.jfrogMigrationForm.basedir = selectedStorage.basedir.split('/')[2]
        }
        this.isExistingStorage = true;
      } else {
        // 如果是手动输入的新值，清空 storageProvider 和 basedir
        this.jfrogMigrationForm.storageProvider = '';
        this.jfrogMigrationForm.basedir = '';
        this.isExistingStorage = false;
      }
    },
    handleContentChange(checkedValues) {
      const allValues = this.contentOptions.map(option => option.value);
      let newCheckedValues = [...checkedValues];
      const previousSet = new Set(this.previousCheckedValues);
      const currentSet = new Set(checkedValues);
      const uncheckedValue = [...previousSet].find(value => !currentSet.has(value));
      if (uncheckedValue) {
        const startIndex = allValues.indexOf(uncheckedValue);
        newCheckedValues = newCheckedValues.filter(value => {
          const index = allValues.indexOf(value);
          return index < startIndex;
        });
      } else {
        newCheckedValues.forEach(value => {
          const currentIndex = allValues.indexOf(value);
          for (let i = 0; i < currentIndex; i++) {
            if (!newCheckedValues.includes(allValues[i])) {
              newCheckedValues.push(allValues[i]);
            }
          }
        });
      }
      this.jfrogMigrationForm.contents = [...new Set(newCheckedValues)].sort((a, b) => {
        return allValues.indexOf(a) - allValues.indexOf(b);
      });
      this.previousCheckedValues = [...this.jfrogMigrationForm.contents];
    },
    // 显示重置 Modal
    showRestoreModal() {
      this.restoreModalVisible = true;
      this.fileList = [];
      this.uploadedFile = null;
    },

    // 关闭重置 Modal
    handleRestoreCancel() {
      this.restoreModalVisible = false;
      this.fileList = [];
      this.uploadedFile = null;
    },
    // 确认重置
    handleRestoreOk() {
      if (!this.fileList[0]) {
        this.$message.warning(this.$t('Setting.PleaseSelectFile'));
        return;
      }

      this.restoreLoading = true;
      const formData = new FormData();
      formData.append('file', this.fileList[0]);

      importJfrog(formData)
        .then(response => {
          this.$message.success(this.$t('Setting.RestoreSuccess'));
          this.handleRestoreCancel();
        })
        .catch(error => {
          const errMsg = error.response ? error.response.data.error : error.message;
          this.$notification.error({
            message: errMsg,
            description: ''
          });
        })
        .finally(() => {
          this.restoreLoading = false;
        });
    },

    // 处理文件上传
    customUploadRequest({ file, onSuccess }) {
      console.log('file',file);
      this.fileList=[];
      this.fileList.push(file);
      onSuccess();
    },

    removeFile(){
      this.fileList=[];
    },
      
    downloadTemplate() {
      downloadTemplate().then(res => {
        const url = window.URL.createObjectURL(new Blob([res]));
        const link = document.createElement('a');
        link.href = url;
        link.setAttribute('download', 'syncRepositoryType.xlsx');
        document.body.appendChild(link);
        link.click();
      })
    }
  },
 
}
</script>

<style lang="scss" scoped></style>
