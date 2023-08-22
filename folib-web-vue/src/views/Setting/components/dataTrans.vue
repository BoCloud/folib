<template>
  <div >
    <a-row type="flex"
    :gutter="24">

<!-- 许可信息-->
<a-col :span="24"
      :md="12"
      class="mb-24">
 <a-card :bordered="false">
  <h6>索引文件获取</h6>
    <div>format=json&indexId=maven-local&chainId=1692595382056&url=http://10.10.33.149:8081/artifactory/maven-local</div>
<a-spin :spinning="spinning" :delay="500">
  <a-form :form="form" :label-col="{ span: 3 }" :wrapper-col="{ span: 12 }" @submit="handleSubmit"  style="margin-top:20px">
    <a-form-item label="格式"  class="dataTrans">
      <a-select
      v-decorator="[
        'format',
        { rules: [{ required: true, message: 'Please select data format!' }] },
      ]"
      placeholder="Please select data format!"
    >
      <a-select-option value="json" >
        json
      </a-select-option>
    </a-select>
    </a-form-item>
    
    <a-form-item label="indexId" class="dataTrans">
      <a-input
      v-decorator="['indexId', { rules: [{ required: true, message: 'Please input data indexId!' }] }]"
      placeholder="Please select data indexId!"
   
    />
    </a-form-item>
    <a-form-item label="chainId" class="dataTrans">
      <a-input
      v-decorator="['chainId', { rules: [{ required: true, message: 'Please input data chainId!' }] }]"
      placeholder="Please select data chainId!"
      
    />
    </a-form-item>
    <a-form-item label="仓库地址"  class="dataTrans">
      <a-input
        v-decorator="['url', { rules: [{ required: true, message: 'Please input data url!' }] }]"
        placeholder="Please select data url!"
       
      />
    </a-form-item>
    

 
    <a-form-item :wrapper-col="{ span: 12, offset: 13 }">
      <a-button type="primary" html-type="submit">
        提交
      </a-button>
    </a-form-item>
  </a-form>
  </a-spin>
  <div v-if="jsonStr" >
  <div class="cardHead"> <h6>索引数据预览</h6>  <span @click="downloadJson(jsonStr)"><a-icon type="arrow-down" theme="outlined" />下载</span> </div>
  <json-viewer :value="jsonStr" :expand-depth="1" boxedsort:show-array-index="false" copyable >
    <template slot="copy">
      <span class="copyText"><a-icon type="copy" theme="twoTone" title="复制" /> 复制</span>
      </template> 
    </json-viewer>
  </div>
  
 </a-card>
</a-col>

<a-col :span="24"
      :md="12"
      class="mb-24">

 <a-card :bordered="false">
  <h6>数据同步</h6>

  <a-form
  id="components-form-demo-validate-other"
  :form="form_dataTran"
  :label-col="{ span: 3 }" :wrapper-col="{ span: 12 }"
  @submit="handleSubmitTrans"
>
   <a-form-item label="下载baseUrl">
    <a-input
    v-decorator="['baseUrl', { rules: [{ required: true, message: 'Please input data baseUrl!' }] }]"
    placeholder="Please select data url!"
  />

   </a-form-item>
  
  <a-form-item label="json上传">
    <div class="dropbox">
      <a-upload-dragger
      name="file"
      v-decorator="[
        'file',
      ]"
      :beforeUpload="beforeUpload"
      :multiple="false"
      :fileList="fileList"
      @change="handleChange"
      accept=".json"
      >
        <p class="ant-upload-drag-icon">
          <a-icon type="inbox" />
        </p>
        <p class="ant-upload-text">
          点击或者拖拽到这里上传
        </p>
        <p class="ant-upload-hint">
          支持上传仓库的索引的json文件
        </p>
      </a-upload-dragger>
    </div>
  </a-form-item>

  <a-form-item :wrapper-col="{ span: 12, offset: 20 }">
    <a-button type="primary" html-type="submit">
      提交
    </a-button>
  </a-form-item>
</a-form>



 </a-card>
</a-col>
    </a-row>
  </div>
</template>
<script>
import JsonViewer from 'vue-json-viewer'
import { getDataIndexDump,uploadJsonFile } from "@/api/settings";
// Importing charts
export default {
  data() {
    return {
      fileList:[],
      jsonStr:null,
      spinning:false,
      formLayout: 'vertical',
      form: this.$form.createForm(this, { name: 'coordinated' }),
      form_dataTran:this.$form.createForm(this, { name: 'validate_other' })

     
    };
  },
  components: {
    JsonViewer
  },
  created() {
  },
  mounted() {
  },
  methods: {
    beforeUpload(e){
      return false
    },
    handleSubmit(e) {
      e.preventDefault();
      this.form.validateFields((err, values) => {
        if (!err) {
          this.spinning=true
          getDataIndexDump(values).then(res=>{
            this.jsonStr=res;
            this.spinning=false
          })
        }
      });
    },


    handleSubmitTrans(e){
      e.preventDefault();
      this.form_dataTran.validateFields((err, values) => {     
        if (!err) {     
         var formData = new FormData();
         formData.append("file",values.file.fileList[0].originFileObj)
         formData.append("baseUrl",values.baseUrl)

          uploadJsonFile(formData).then(o=>{
            console.log(o);
          })
        }

        
          })
     
    },

    downloadJson(data){
      if (!data) {
        message('数据为空');
        return;
    }
    let filename = 'json.json'
    if (typeof data === 'object') {
        data = JSON.stringify(data, undefined, 4)
    }
    var blob = new Blob([data], { type: 'text/json' });
    var a = document.createElement('a');
    a.download = filename;
    a.href = window.URL.createObjectURL(blob);
    a.dataset.downloadurl = ['text/json', a.download, a.href].join(':');
    a.click()
    },
    

     handleChange(info) {
      this.fileList=info.fileList
    },

   
  
  },

};
</script>
<style lang="scss" scoped>
.dataTrans  .ant-form-item-label{
  line-height:70px !important;
}
.cardHead{
  display: flex;
  justify-content:flex-start;

  span{
    margin-left: 15px;
    color: #1890FF;
    cursor: pointer;
  }
  span:hover{
    text-decoration: underline;
  }
}

.copyText{
  color: #1890FF;
  cursor: pointer;
  &:hover{
    text-decoration: underline;
  }
}
</style>