<template>
  <div>
    <div v-for="(i, index) in cronCanSetList" :key="index">
      <a-row type="flex" align="middle">
        <a-col style="min-width: 40px;" class="text-center">
          <a-icon type="clock-circle" class="text-gray-6" style="font-size: 18px;" />
        </a-col>
        <a-col class="pl-15">
          <p class="mb-0">{{ i.name }}</p>
          <small class="text-dark">{{ i.description }}</small>
        </a-col>
        <a-col :span="24" :md="12" class="ml-auto"
          style="display: flex; align-items: center; justify-content: flex-end">
          <a-tag v-if="i.isSetted && i.isSetted.uuid" color="success" class="ant-tag-success font-bold">已设定
          </a-tag>
          <span class="ml-5">{{ i.scope }}</span>
          <a-button @click="cronShowHandle(i, index)" type="link" class="btn-more ml-5">
            展开设定
            <a-icon :type="i.isShow ? 'arrow-down' : 'arrow-right'" />
          </a-button>
        </a-col>
      </a-row>
      <a-card v-if="i.isShow" :bordered="false" class="bg-gray-3 shadow-0 mb-24"
        :bodyStyle="{ padding: '8px' }">
        <a-row type="flex" align="middle">
          <a-col>
            <p class="font-semibold mb-0 ml-10">{{ i.isSetted.jobClass }}</p>
          </a-col>
          <a-col class="ml-auto">
            <a-input v-model="i.isSetted.cronExpression" size="small" class="font-regular text-sm text-dark"
              style="width: 100px;" />
          </a-col>
          <a-col class="ml-auto">
            <span class="mr-15">{{ i.isSetted.oneTimeExecution ? '执行一次' : '循环执行' }}</span>
            <a-switch v-model="i.isSetted.oneTimeExecution"
              @change="oneTimeExecutionChange($event, i.isSetted)" />
          </a-col>
          <a-col class="ml-auto">
            <span class="mr-15">{{ i.isSetted.immediateExecution ? '立即执行' : '不立即执行' }}</span>
            <a-switch v-model="i.isSetted.immediateExecution"
              @change="immediateExecutionChange($event, i.isSetted)" />
          </a-col>
        </a-row>
        <hr v-if="i.fields.length > 2" class="gradient-line my-10">
        <a-row type="flex" align="middle">
          <a-col v-if="i.fields.length > 2" style="margin-right: 15px">
            <p class="font-semibold mb-0 ml-10">其他参数:</p>
          </a-col>
          <div v-if="i.fields.length > 2">
            <div v-for="(f, index) in i.fields" :key="index">
              <a-col v-if="f.name !== 'storageId' && f.name !== 'repositoryId'" class="ml-auto">
                <span style="margin-left: 15px" class="mr-15">{{ f.name }}</span>
                <a-input v-if="f.type === 'string'" v-model="f.value" size="small"
                  class="font-regular text-sm text-dark" style="width: 250px;" />
                <a-input-number v-if="f.type === 'int' && f.name === 'numberToKeep'" v-model="f.value"
                  size="small" class="font-regular text-sm text-dark" style="width: 120px;" />
                <a-input-number :min="1" v-if="f.type === 'int' && f.name === 'storageDay'" v-model="f.value"
                                size="small" class="font-regular text-sm text-dark" style="width: 120px;" />
                <a-date-picker v-if="f.type === 'int' && f.name === 'keepPeriod'" v-model="f.value"
                  size="small" class="font-regular text-sm text-dark" style="width: 120px;" />
                <a-switch v-if="f.type === 'boolean'" v-model="f.value" @change="() => { $forceUpdate() }" />
              </a-col>
            </div>
          </div>
        </a-row>
        <a-row :gutter="[24]">
          <a-col :span="12">
          </a-col>
          <a-col :span="12" class="text-right">
            <a-button @click="saveCronOneSetHandle(i)" type="primary" size="small" shape="circle"
              icon="save" />
            <a-button v-if="i.isSetted.uuid" @click="delCronOneSetHandle(i)" style="margin-left: 15px"
              type="danger" size="small" shape="circle" icon="delete" />
          </a-col>
        </a-row>
      </a-card>
      <hr class="gradient-line my-10">
    </div>
  </div>
</template>
<script>
import {
  crontasksList,
  crontasksByRepository,
  creatCronOne,
  updateCronOne,
  delCronOne,
} from "@/api/folib"

export default {
  props: { 
		folibRepository: {
			type: Object,
			default: {},
		},
	},
  data() {
    return {
      cronCanSetList: [],
      cronSettedList: [],
    }
  },
  components: {
    
  },
  created() {
    this.resetData()
    this.crontasksListHandle()
  },
  mounted() {},
  methods: {
    resetData() {
      this.cronCanSetList = []
      this.cronSettedList = []
    },
    crontasksListHandle() {
      crontasksList(this.folibRepository.layout === 'Maven 2' ? 'MAVEN' : this.folibRepository.layout.toUpperCase()).then(res => {
        this.cronCanSetList = res
        crontasksByRepository(this.folibRepository.storageId, this.folibRepository.id).then(res => {
          //已经被设置的定时任务列表
          this.cronSettedList = res.cronTaskConfigurations
          //当前仓库可设置的全量列表
          this.cronCanSetList.forEach(c => {
            c.isShow = false
            c.isSetted = { jobClass: c.jobClass, cronExpression: '0 0 2 * * ?', oneTimeExecution: true, immediateExecution: false }
            //循环给fields添加
            c.fields.forEach(o => {
              if (o.name === 'storageId') {
                o.value = this.folibRepository.storageId
              } else if (o.name === 'repositoryId') {
                o.value = this.folibRepository.id
              }
            })
            //将已经设置好的properties写入给fields，便于后续update
            this.cronSettedList.forEach(s => {
              if (c.jobClass === s.jobClass) {
                c.isSetted = s;
                for (let key in s.properties) {
                  c.fields.forEach(o => {
                    if (o.name === key) {
                      o.value = s.properties[key] === 'true' ? true : s.properties[key] === 'false' ? false : s.properties[key]
                    }
                  })
                }
              }
            })
          })
          this.$forceUpdate()
        })
      })
    },
    cronShowHandle(i, index) {
      if (i.isShow) {
        i.isShow = false
      } else {
        i.isShow = true
        this.cronCanSetList.splice(index, i)
      }
      this.$forceUpdate()

    },
    delCronOneSetHandle(i) {
      delCronOne(i.isSetted.uuid).then(res => {
        setTimeout(() => {
          this.$notification.open({
            class: 'ant-notification-success',
            message: '成功',
            description: res,
          });
        }, 100)
      })
      this.crontasksListHandle()
    },
    saveCronOneSetHandle(i) {
      if (i.fields && i.isSetted) {
        if (!i.isSetted.cronExpression) {
          this.$notification.open({
            class: 'ant-notification-warning',
            message: '操作不正确',
            description: '请填写cron表达式',
          })
          return false
        }
        let fiedsNew = []
        i.fields.forEach(f => {
          if (f.value !== null && f.value !== undefined) {
            fiedsNew.push({ name: f.name, value: f.value })
          }
        })
        i.isSetted.fields = fiedsNew
        if (i.isSetted.uuid) {
          let uuid = i.isSetted.uuid
          delete i.isSetted.uuid
          delete i.isSetted.name
          delete i.isSetted.properties
          updateCronOne(i.isSetted, uuid).then(res => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-success',
                message: '成功',
                description: res,
              });
            }, 100)
          }).catch((err) => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: '失败',
                description: err.response.data.error,
              });
            }, 100)

          })
        } else {
          creatCronOne(i.isSetted).then(res => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-success',
                message: '成功',
                description: res,
              });
            }, 100)
          }).catch((err) => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: '失败',
                description: err.response.data.error,
              });
            }, 100)

          })
        }
      }
      this.crontasksListHandle()
    },
  },
};
</script>