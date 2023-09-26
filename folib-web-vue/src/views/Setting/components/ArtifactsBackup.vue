<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-form :form="backupForm" ref="backupForm" layout="horizontal" :hideRequiredMark="true">
          <a-row :gutter="[24]">
            <a-col :span="24">
              <a-row :gutter="[24]">
                <a-col :span="24">
                  <a-form-item label="备份仓库">
                    <gb-ant-select-two-cascader
                      v-decorator="[
                        'repositoryIds',
                        {
                          initialValue: undefined,
                          rules: [{ required: true, message: '请选择备份仓库', type: 'array',}]
                        }
                      ]"
                      allowClear
                      style="width:40%;"
                      :maxTagCount="4"
                      :maxTagTextLength="12"
                      placeholder="请选择备份仓库"
                      :selectOptionsConfig="{
                        key: 'key',
                        value: 'key',
                        text: 'id',
                        children: 'children'
                      }"
                      dropdownClassName="customer-multiple-cascader"
                      :treeData="repositories"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="24">
                  <a-form-item class="mb-10" label="备份目录" :colon="false" prop="repositoryId">
                    <a-tree-select
                      v-decorator="[
                        'directoryPath',
                        {
                          initialValue: undefined,
                          rules: [{ required: true, message: '请选择备份目录',}]
                        }
                      ]"
                      style="width: 40%"
                      :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }"
                      :tree-data="directoryPaths"
                      placeholder="请选择备份目录"
                      :load-data="onLoadData"
                    />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-form-item :wrapper-col="{ span: 14, offset: 6 }">
                    <a-popconfirm title="确定要开启备份吗？" okType="danger" ok-text="确定" cancel-text="取消"
                      @confirm="backupFormSubmit">
                      <a-button type="danger">
                        保存
                      </a-button>
                    </a-popconfirm>
                    <a-button class="ml-10" @click="dataMigrationResetForm">
                      取消
                    </a-button>
                  </a-form-item>
                </a-col>
              </a-row>
            </a-col>
          </a-row>
        </a-form>
      </a-col>
    </a-row>
  </div>
</template>
<script>
import {
  getStoragesAndRepositories
} from "@/api/folib"
import {
  folderList,
} from "@/api/advanced"

export default {
  inject: ["reload"],
  data() {
    return {
      repositories: [],
      directoryPaths: [],
      backupForm: this.$form.createForm(this, { name: 'backupForm' }),
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

  },
  mounted() { },
  methods: {
    message(type, message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification[type]({
        message: message,
        description: "",
      })
    },
    initData() {
      this.queryStoragesAndRepositories()
      this.folderList()
    },
    queryStoragesAndRepositories() {
      getStoragesAndRepositories({ excludeType: 'group' }).then(res => {
        if (res) {
          this.repositories = []
          res.forEach(item => {
            if (item.children && item.children.length > 0)
            {
              this.repositories.push(item)
            }
          })
        }
      })
    },
    folderList() {
      folderList({ directoryPath: "/" }).then(res => {
        if (res) {
          this.directoryPaths = []
          res.forEach(item => {
            this.directoryPaths.push({
              key: item.fullPath,
              title: item.name,
              value: item.fullPath,
              isLeaf: !item.hasSubDirectories
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
        folderList({directoryPath: treeNode.dataRef.value}).then((res) => {
          if (res) {
            res.forEach((item) => {
              let obj = {}
              obj = {
                key: item.fullPath,
                title: item.name,
                value: item.fullPath,
                isLeaf: !item.hasSubDirectories
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
    backupFormSubmit() {
      this.backupForm.validateFields((err, values) => {
        if (!err){
          console.log(JSON.stringify(values))
          this.backupFormReset()
          this.message("success","备份策略设置成功")
          // syncArtifactProvider(this.dataMigrationForm).then(res => {
          //   if (res) {
          //     setTimeout(() => {
          //       this.getSingleDict("handler_maven_indexer")
          //     }, 100)
          //     this.message("success", "请稍等，数据迁移任务已启动，正在异步执行")
          //   }
          // }).catch((err) => {
          //   this.message("error", "执行数据迁移任务失败")
          // }).finally(() => {
          //   this.dataMigrationResetForm()
          // })
        }
      })
    },
    backupFormReset() {
      this.backupForm.resetFields()
      this.initData()
    },
  }
}
</script>

<style lang="scss" scoped></style>