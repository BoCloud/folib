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
          <a-tag v-if="i.isSetted && i.isSetted.uuid" color="success" class="ant-tag-success font-bold">
            {{ $t('Cron.HasBeenSet') }}
          </a-tag>
          <span class="ml-5">{{ i.scope }}</span>
          <a-button @click="cronShowHandle(i, index)" type="link" class="btn-more ml-5">
            {{ $t('Cron.ExpandSettings') }}
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
            <span class="mr-15">{{ i.isSetted.oneTimeExecution ? $t('Cron.DoItOnce') : $t('Cron.LoopExecution') }}</span>
            <a-switch v-model="i.isSetted.oneTimeExecution"
              @change="oneTimeExecutionChange($event, i.isSetted)" />
          </a-col>
          <a-col class="ml-auto">
            <span class="mr-15">{{ i.isSetted.immediateExecution ? $t('Cron.ExecuteImmediately') : $t('Cron.NotExecutedImmediately') }}</span>
            <a-switch v-model="i.isSetted.immediateExecution"
              @change="immediateExecutionChange($event, i.isSetted)" />
          </a-col>
        </a-row>
        <hr v-if="i.fields.length > 2" class="gradient-line my-10">
        <a-row type="flex" align="middle">
          <a-col v-if="i.fields.length > 2" style="margin-right: 15px">
            <p class="font-semibold mb-0 ml-10">{{ $t('Cron.OtherParameters') }}</p>
          </a-col>
          <div v-if="i.fields.length > 2">
            <div v-for="(f, index) in i.fields" :key="index" class="mt-10">
              <a-col v-if="f.name !== 'storageId' && f.name !== 'repositoryId' && f.name !=='storageCondition'" class="ml-auto">
                <span style="margin-left: 15px" class="mr-15" v-if="f.aliasName && f.aliasName.length > 0">{{ f.aliasName }}</span>
                <span style="margin-left: 15px" class="mr-15" v-else>{{ f.name }}</span>
                <a-input v-if="f.type === 'string'" v-model="f.value" size="small"
                  class="font-regular text-sm text-dark" style="width: 250px;" />
                <a-input-number :min="1" v-if="f.type === 'int' && f.name === 'numberToKeep'" v-model="f.value"
                  size="small" class="font-regular text-sm text-dark" style="width: 120px;" />
                <a-input-number :min="1" v-if="f.type === 'int' && f.name === 'storageDay'" v-model="f.value"
                                size="small" class="font-regular text-sm text-dark" style="width: 120px;"/>  
                <a-input-number :min="1" v-if="f.type === 'int' && f.name === 'keepPeriod'" v-model="f.value"
                  size="small" class="font-regular text-sm text-dark" style="width: 120px;" />
                <a-switch v-if="f.type === 'boolean'" v-model="f.value" @change="() => { $forceUpdate() }" />
              </a-col>
              <a-col v-if="folibRepository.layout.toLowerCase() === 'docker' && f.name ==='storageCondition'" class="ml-auto">
                <span style="margin-left: 15px" class="mr-15" v-if="f.aliasName && f.aliasName.length > 0">{{ f.aliasName }}</span>
                <span style="margin-left: 15px" class="mr-15" v-else>{{ f.name }}</span>
                <a-select v-model="f.value" style="width: 120px" @change="storageConditionChange($event, i.fields)">
                  <a-select-option v-for="(item, index) in i18nStorageConditions"
                  :label="item.label"
                  :key="index"
                  :value="item.value">
                  {{ item.label }}
                  </a-select-option>
                </a-select>
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
      storageConditions: [
        {
          label: "Tag",
          value: "tag"
        },
        {
          label: '天数',
          i18nKey: 'Cron.Days',
          value: "day"
        }
      ],
    }
  },
  components: {
    i18nStorageConditions() {
      return this.storageConditions.map(column => {
        if (column.i18nKey) {
          column.label = this.$t(column.i18nKey);
        }
        return column;
      })
    }
  },
  created() {
    this.resetData()
    this.crontasksListHandle()
  },
  mounted() {

  },
  methods: {
    resetData() {
      this.cronCanSetList = []
      this.cronSettedList = []
    },
    storageConditionChange(event, fields) {
      let aliasName = this.$t('Cron.RetentionDaysNum')
      if (event === 'tag') {
        aliasName = this.$t('Cron.KeepTheNumber')
      } else if (event === 'day') {
        aliasName = this.$t('Cron.RetentionDaysNum')
      }
      fields.filter(i => i.name === "storageDay").forEach(i => i.aliasName = aliasName)
      this.$forceUpdate()
    },
    crontasksListHandle() {
      crontasksList(this.folibRepository.layout === 'Maven 2' ? 'MAVEN' : this.folibRepository.layout.toUpperCase()).then(res => {
        this.cronCanSetList = res

        // Cocoapods: 本地仓库过滤掉代理仓库定时任务
        if (this.folibRepository.type === "hosted")
        { this.cronCanSetList = this.cronCanSetList.filter(e => !(e.jobClass === "com.veadan.folib.cron.jobs.SyncProxyRepositoryIndexCronJob")) }

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
        this.$forceUpdate()
      })
    },
    cronShowHandle(i, index) {
      if (i.isShow) {
        i.isShow = false
      } else {
        i.isShow = true
        this.cronCanSetList.splice(index, i)
        if (this.folibRepository.layout.toLowerCase() === "docker") {
          let storageCondition = 'day'
          let cleanupTask = i.fields.filter(i => i.name === 'storageCondition')
          if (cleanupTask && cleanupTask.length >0) {
            storageCondition = cleanupTask[0].value
          }
          this.storageConditionChange(storageCondition, i.fields)
        }
      }
      this.$forceUpdate()

    },
    delCronOneSetHandle(i) {
      delCronOne(i.isSetted.uuid).then(res => {
        setTimeout(() => {
          this.$notification.open({
            class: 'ant-notification-success',
            message: this.$t('Cron.Success'),
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
            message: this.$t('Cron.TheOperationIsIncorrect'),
            description: this.$t('Cron.FillInTheCronExpression'),
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
                message: this.$t('Cron.Success'),
                description: res,
              });
            }, 100)
          }).catch((err) => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Cron.Failure'),
                description: err.response.data.error,
              });
            }, 100)

          })
        } else {
          creatCronOne(i.isSetted).then(res => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-success',
                message: this.$t('Cron.Success'),
                description: res,
              });
            }, 100)
          }).catch((err) => {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: this.$t('Cron.Failure'),
                description: err.response.data.error,
              });
            }, 100)

          })
        }
      }
      this.crontasksListHandle()
    },
    oneTimeExecutionChange(value, item) {
      if (value && item.immediateExecution) {
        item.immediateExecution = false
      }
      this.$forceUpdate()
    },
    immediateExecutionChange(value, item) {
      if (value && item.oneTimeExecution) {
        item.oneTimeExecution = false
      }
      this.$forceUpdate()
    },
  },
};
</script>
