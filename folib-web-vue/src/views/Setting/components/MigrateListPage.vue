<template>
  <div>
    <a-tabs v-model="activeTab">
      <a-tab-pane :key="1" :tab="$t('Setting.PendingMigration')">
        <div class="table-operations-container">
          <div class="table-operations">
            <a-button type="primary" @click="handleAdd">
              {{ $t('Setting.AddRepository') }}
            </a-button>
            <a-button 
              type="primary" 
              :disabled="!hasSelectedRows"
              @click="handleStartMigration"
            >
              {{ $t('Setting.StartMigration') }}
            </a-button>
          </div>
        </div>
        
        <a-table
          :columns="getPendingColumns()"
          :data-source="pendingData"
          :pagination="pendingPagination"
          :row-selection="pendingRowSelection"
          @change="(pagination) => handleTableChange(pagination, 'pending')"
          :loading="pendingLoading"
          :row-key="record => `${record.storageId}:${record.repositoryId}`"
        >
          <template slot="status" slot-scope="text, record">
            <a-tag :color="getStatusColor(record.syncStatus)">
              {{ getStatusText(record.syncStatus) }}
            </a-tag>
          </template>
          <template slot-scope="text" slot="usedSpace">
            {{ text ? text : $t('Setting.Unknown') }}
          </template>
          <template slot-scope="text, record" slot="postLayout">
            <a-select style="width: 120px" :value="text" @change="handlePostLayoutChange(record, $event)" >
              <a-select-option v-for="option in layoutOptions" :key="option.value" :value="option.value">
                {{ option.value }}
              </a-select-option>
            </a-select>
          </template>
        </a-table>

        <a-modal
          :visible="addModalVisible"
          :title="$t('Setting.AddRepository')"
          @ok="handleAddOk"
          @cancel="handleAddCancel"
          :confirmLoading="addLoading"
        >
          <a-form-model :model="addForm" :rules="addRules" ref="addFormRef">
            <a-form-model-item :label="$t('Setting.StorageId')" prop="storageId">
              <a-select 
                v-model="addForm.storageId" 
                @change="handleStorageChange"
                showSearch
                :filterOption="filterOption"
                :placeholder="$t('Setting.PleaseSelectStorage')"
              >
                <a-select-option v-for="storage in storageOptions" :key="storage.id">
                  {{ storage.id }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
            
            <a-form-model-item :label="$t('Setting.RepositoryId')" prop="repositoryIds">
              <a-select 
                v-model="addForm.repositoryIds"
                mode="multiple"
                :loading="repositoryLoading"
                :disabled="!addForm.storageId"
                :placeholder="$t('Setting.PleaseSelectRepository')"
                :filter-option="filterOption"
                style="width: 100%"
                allowClear
                :maxTagCount="3"
                @change="handleSelectChange"
              >
                <a-select-option v-for="repo in repositoryOptions" :key="repo.id">
                  {{ repo.id }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-form-model>
        </a-modal>
      </a-tab-pane>

      <a-tab-pane :key="2" :tab="$t('Setting.Migrating')">
        <div class="table-operations-container">
          <div class="table-operations">
            <a-button 
              type="primary" 
              @click="handleSetFailed"
            >
              {{ $t('Setting.setFailed') }}
            </a-button>
           <!-- <a-button 
              type="primary" 
              :disabled="!canContinueMigration"
              @click="handleContinueMigration"
            >
              {{ $t('Setting.ContinueMigration') }}
            </a-button>  -->
            <!-- <a-button 
              type="primary" 
              :disabled="!canPauseMigration"
              @click="handlePauseMigration"
            >
              {{ $t('Setting.PauseMigration') }}
            </a-button> -->
          </div>
        </div>
        
        <a-table
          :columns="getMigratingColumns()"
          :data-source="migratingData"
          :pagination="migratingPagination"
          :row-selection="migratingRowSelection"
          @change="(pagination) => handleTableChange(pagination, 'migrating')"
          :loading="migratingLoading"
          :row-key="record => `${record.storageId}:${record.repositoryId}`"
        >
          <template slot="status" slot-scope="text, record">
            <a-tag :color="getStatusColor(record.syncStatus)">
              {{ getStatusText(record.syncStatus) }}
            </a-tag>
          </template>
          <template slot-scope="text,record" slot="progress">
            <a-progress :percent="record.progress || 0" />
          </template>
          <template slot-scope="text" slot="usedSpace">
            {{ text ? text : $t('Setting.Unknown') }}
          </template>
          <template slot="totalArtifact" slot-scope="text,record">
            {{ text }}
          </template>
        </a-table>
      </a-tab-pane>

      <a-tab-pane :key="3" :tab="$t('Setting.Completed')">
        <div class="table-operations-container">
          <div class="table-operations">
            <a-button 
              type="primary" 
              :disabled="!hasCompletedSelectedRows"
              @click="handleComplete"
            >
              {{ $t('Setting.FinishMigration') }}
            </a-button>
          </div>
        </div>

        <a-table
          :columns="getCompletedColumns()"
          :data-source="completedData"
          :pagination="completedPagination"
          :row-selection="completedRowSelection"
          @change="(pagination) => handleTableChange(pagination, 'completed')"
          :loading="completedLoading"
          :row-key="record => `${record.storageId}:${record.repositoryId}`"
        >
          <template slot="status" slot-scope="text, record">
            <a-tag :color="getStatusColor(record.syncStatus)">
              {{ getStatusText(record.syncStatus) }}
            </a-tag>
          </template>
          <template slot-scope="text" slot="endTime">
            {{ text || '--' }}
          </template>
          <template slot-scope="text" slot="usedSpace">
            {{ text ? text : $t('Setting.Unknown') }}
          </template>
        </a-table>
      </a-tab-pane>
    </a-tabs>

    <div class="page-header">
      <a-button type="link" @click="$emit('back')">
        <a-icon type="arrow-left" />
        {{ $t('Setting.Back') }}
      </a-button>
    </div>
  </div>
</template>



<script>
import { getRepositories, addMigrateRepo ,startMigrate, pauseMigrate, finishMigrate ,continueMigrate,getMigrateProgress,changeLayout,setFailed,getIndexProgress} from '@/api/migrate'
import { getStorages,queryRepositoriesByStorage } from '@/api/folib'

export default {
  name: 'MigrateListPage',
  props: {
    migrateId: {
      type: [String, Number],
      required: true
    }
  },
  data() {
    return {
      activeTab: 1,
      // 待迁移仓库数据
      pendingData: [],
      pendingLoading: false,
      pendingPagination: {
        current: 1,
        pageSize: 10,
        total: 0,
        showTotal: total => `${this.$t('Setting.Total')} ${total} ${this.$t('Setting.Items')}`
      },

      // 迁移中仓库数据
      migratingData: [],
      migratingLoading: false,
      migratingPagination: {
        current: 1,
        pageSize: 10,
        total: 0,
        showTotal: total => `${this.$t('Setting.Total')} ${total} ${this.$t('Setting.Items')}`
      },

      // 已完成仓库数据
      completedData: [],
      completedLoading: false,
      completedPagination: {
        current: 1,
        pageSize: 10,
        total: 0,
        showTotal: total => `${this.$t('Setting.Total')} ${total} ${this.$t('Setting.Items')}`
      },

      selectedPendingKeys: [],
      selectedMigratingKeys: [],
      selectedPendingRows: [],
      selectedMigratingRows: [],
      addModalVisible: false,
      addLoading: false,
      addForm: {
        storageId: undefined,
        repositoryIds: []
      },
      addRules: {
        storageId: [
          { required: true, message: this.$t('Setting.PleaseSelectStorage'), trigger: 'change' }
        ],
        repositoryIds: [
          { 
            type: 'array',
            required: true, 
            message: this.$t('Setting.PleaseSelectRepository'), 
            trigger: 'change' 
          }
        ]
      },
      storageOptions: [],
      repositoryOptions: [],
      repositoryLoading: false,
      checkAll: false,
      indeterminate: false,
      pollingTimer: null,
      pollingInterval: 5000, // 轮询间隔，5秒
      selectedCompletedKeys: [],
      selectedCompletedRows: [],
      layoutOptions: [
        { value: 'raw' },
        { value: 'maven' },
        { value: 'ivy'},
        { value: 'sbt' },
        { value: 'gradle' },
        { value: 'docker' },
        { value: 'helm' },
        { value: 'pypi' },
        { value: 'npm' },
        { value: 'ohpm' },
        { value: 'cocoaPods' },
        { value: 'go' },
        { value: 'php' },
        { value: 'conan' },
        { value: 'nuget' },
        { value: 'rpm' },
        { value: 'gitlfs' },
        { value: 'HuggingFace' },
        { value: 'pub' },
        { value: 'debian' },
        { value: 'ollama' },
      ]
    }
  },
  computed: {
    pendingRowSelection() {
      return {
        selectedRowKeys: this.selectedPendingKeys,
        onChange: (selectedRowKeys, selectedRows) => {
          this.selectedPendingKeys = selectedRowKeys;
          const newSelectedRows = [...this.selectedPendingRows];
          selectedRows.forEach(row => {
            const rowKey = `${row.storageId}:${row.repositoryId}`;
            if (!newSelectedRows.find(item => `${item.storageId}:${item.repositoryId}` === rowKey)) {
              newSelectedRows.push(row);
            }
          });
          this.selectedPendingRows = newSelectedRows.filter(row => 
            selectedRowKeys.includes(`${row.storageId}:${row.repositoryId}`)
          );
        }
      }
    },
    migratingRowSelection() {
      return {
        selectedRowKeys: this.selectedMigratingKeys,
        onChange: (selectedRowKeys, selectedRows) => {
          this.selectedMigratingKeys = selectedRowKeys;
          const newSelectedRows = [...this.selectedMigratingRows];
          selectedRows.forEach(row => {
            const rowKey = `${row.storageId}:${row.repositoryId}`;
            if (!newSelectedRows.find(item => `${item.storageId}:${item.repositoryId}` === rowKey)) {
              newSelectedRows.push(row);
            }
          });
          this.selectedMigratingRows = newSelectedRows.filter(row => 
            selectedRowKeys.includes(`${row.storageId}:${row.repositoryId}`)
          );
        }
      }
    },
    completedRowSelection() {
      return {
        selectedRowKeys: this.selectedCompletedKeys,
        onChange: (selectedRowKeys, selectedRows) => {
          this.selectedCompletedKeys = selectedRowKeys;
          const newSelectedRows = [...this.selectedCompletedRows];
          selectedRows.forEach(row => {
            const rowKey = `${row.storageId}:${row.repositoryId}`;
            if (!newSelectedRows.find(item => `${item.storageId}:${item.repositoryId}` === rowKey)) {
              newSelectedRows.push(row);
            }
          });
          this.selectedCompletedRows = newSelectedRows.filter(row => 
            selectedRowKeys.includes(`${row.storageId}:${row.repositoryId}`)
          );
        }
      }
    },
    hasSelectedRows() {
      return this.activeTab === 1 ? 
        this.selectedPendingKeys.length > 0 : 
        this.selectedMigratingKeys.length > 0;
    },
    // 是否全选
    isAllSelected() {
      return this.repositoryOptions.length > 0 && 
             this.addForm.repositoryIds.length === this.repositoryOptions.length;
    },
    // 是否部分选中
    isIndeterminate() {
      return this.addForm.repositoryIds.length > 0 && 
             this.addForm.repositoryIds.length < this.repositoryOptions.length;
    },
    // 检查是否可以继续迁移（状态为失败或暂停的）
    canContinueMigration() {
      if (this.activeTab !== 2 || this.selectedMigratingRows.length === 0) return false;
      
      // 检查是否所有选中的行都是失败、暂停或排队状态
      const validStatus = [1, 4, 6, 7]; // 排队、暂停、同步索引失败、同步制品失败
      const allRowsValid = this.selectedMigratingRows.every(row => validStatus.includes(row.syncStatus));
      const anyOtherStatus = this.selectedMigratingRows.some(row => !validStatus.includes(row.syncStatus));
      
      return allRowsValid && !anyOtherStatus;
    },
    
    // 检查是否可以暂停迁移（状态为同步制品中的）
    canPauseMigration() {
      if (this.activeTab !== 2 || this.selectedMigratingRows.length === 0) return false;
      
      // 检查是否所有选中的行都是同步制品状态
      const validStatus = [3]; // 同步制品中
      const allRowsValid = this.selectedMigratingRows.every(row => validStatus.includes(row.syncStatus));
      const anyOtherStatus = this.selectedMigratingRows.some(row => !validStatus.includes(row.syncStatus));
      
      return allRowsValid && !anyOtherStatus;
    },
    hasCompletedSelectedRows() {
      return this.activeTab === 3 && this.selectedCompletedKeys.length > 0;
    },
  },
  watch: {
    activeTab: {
      handler(newVal) {
        // 在迁移中标签页启动轮询，其他标签页停止轮询
        if (newVal === 2) {
          this.startPolling();
        } else {
          this.stopPolling();
        }
        // 加载对应标签页的数据
        this.loadData(newVal);
      },
      immediate: true
    },
    'addForm.repositoryIds': {
      handler(value) {
        if (value && this.repositoryOptions.length > 0) {
          this.checkAll = value.length === this.repositoryOptions.length;
          this.indeterminate = value.length > 0 && value.length < this.repositoryOptions.length;
        }
      },
      immediate: true
    }
  },
  mounted() {
    this.loadStorages();
    this.loadPendingData();

  },
  methods: {
    clearSelection() {
      this.selectedPendingKeys = [];
      this.selectedMigratingKeys = [];
      this.selectedCompletedKeys = [];
      this.selectedPendingRows = [];
      this.selectedMigratingRows = [];
      this.selectedCompletedRows = [];
    },
    async loadData(tab) {
      // 清空之前的数据
      this.clearData();
      
      switch (tab) {
        case 1:
          await this.loadPendingData();
          break;
        case 2:
          await this.updateMigratingProgress();
          break;
        case 3:
          await this.loadCompletedData();
          break;
      }
    },
    // 清空数据的方法
    clearData() {
      // 清空选中状态
      this.clearSelection();
      // 清空表格数据
      this.pendingData = [];
      this.migratingData = [];
      this.completedData = [];
      // 重置分页
      this.pendingPagination.current = 1;
      this.migratingPagination.current = 1;
      this.completedPagination.current = 1;
    },
    async loadPendingData() {
      this.pendingLoading = true;
      try {
        const response = await getRepositories({
          page: this.pendingPagination.current,
          limit: this.pendingPagination.pageSize,
          status: 'pending',
          migrateId: this.migrateId
        });
        if (response?.data) {
          this.pendingData = response.data.rows;
          this.pendingPagination.total = response.data.total;
        }
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      } finally {
        this.pendingLoading = false;
      }
    },
    async loadMigratingData(showLoading = true) {
      if (showLoading) {
        this.migratingLoading = true;
      }
      try {
        const response = await getRepositories({
          page: this.migratingPagination.current,
          limit: this.migratingPagination.pageSize,
          status: 'migrating',
          migrateId: this.migrateId
        });
        if (response?.data) {
          this.migratingData = response.data.rows;
          this.migratingPagination.total = response.data.total;
        }
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      } finally {
        if (showLoading) {
          this.migratingLoading = false;
        }
      }
    },
    async loadCompletedData() {
      this.completedLoading = true;
      try {
        const response = await getRepositories({
          page: this.completedPagination.current,
          limit: this.completedPagination.pageSize,
          status: 'completed',
          migrateId: this.migrateId
        });
        if (response?.data) {
          this.completedData = response.data.rows;
          this.completedPagination.total = response.data.total;
        }
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      } finally {
        this.completedLoading = false;
      }
    },
    handleTableChange(pagination, type) {
      switch (type) {
        case 'pending':
          this.pendingPagination.current = pagination.current;
          this.loadPendingData();
          break;
        case 'migrating':
          this.migratingPagination.current = pagination.current;
          this.updateMigratingProgress();
          break;
        case 'completed':
          this.completedPagination.current = pagination.current;
          this.loadCompletedData();
          break;
      }
    },
    getStatusColor(status) {
      const statusMap = {
        0: 'blue',          // 初始
        1: 'orange',        // 排队
        2: 'cyan',          // 获取索引
        3: 'geekblue',      // 同步制品
        4: 'gold',          // 暂停
        5: 'green',         // 完成
        6: 'red',           // 同步索引失败
        7: 'red'            // 同步制品失败
      };
      return statusMap[status] || 'blue';
    },

    getStatusText(status) {
      const statusMap = {
        0: this.$t('Setting.Initial'),         // 初始
        1: this.$t('Setting.Queuing'),         // 排队
        2: this.$t('Setting.FetchingIndex'),   // 获取索引
        3: this.$t('Setting.SyncingArtifact'), // 同步制品
        4: this.$t('Setting.Paused'),          // 暂停
        5: this.$t('Setting.Completed'),       // 完成
        6: this.$t('Setting.IndexFailed'),     // 同步索引失败
        7: this.$t('Setting.SyncingFailed')    // 同步制品失败
      };
      return statusMap[status] || status;
    },
    // 待迁移列
    getPendingColumns() {
      return [
      {
          title: this.$t('Setting.StorageId'),
          dataIndex: 'storageId',
          align: 'center',
          key: 'storageId'
        },
        {
          title: this.$t('Setting.RepositoryId'),
          dataIndex: 'repositoryId',
          align: 'center',
          key: 'repositoryId'
        },
        {
          title: this.$t('Setting.UsedSpace'),
          dataIndex: 'usedSpace',
          align: 'center',
          key: 'usedSpace',
          scopedSlots: { customRender: 'usedSpace' }
        },
        {
            title: this.$t('Setting.PreLayout'),
            dataIndex: 'layout',
            align: 'center',
            key: 'layout'
        },
        {
            title: this.$t('Setting.PostLayout'),
            dataIndex: 'postLayout',
            align: 'center',
            key: 'postLayout',
            scopedSlots: { customRender: 'postLayout' }
        },
        {
          title: this.$t('Setting.Status'),
          dataIndex: 'syncStatus',
          align: 'center',
          key: 'syncStatus',
          scopedSlots: { customRender: 'status' }
        },
        
      ]
    },
    // 迁移中列
    getMigratingColumns() {
      return [
      {
          title: this.$t('Setting.StorageId'),
          dataIndex: 'storageId',
          align: 'center',
          key: 'storageId'
        },
        {
          title: this.$t('Setting.RepositoryId'),
          dataIndex: 'repositoryId',
          align: 'center',
          key: 'repositoryId'
        },
        {
          title: this.$t('Setting.TotalArtifact'),
          dataIndex: 'totalArtifact',
          key: 'totalArtifact',
          align: 'center',
          scopedSlots: { customRender: 'totalArtifact' }
        },
        {
            title: this.$t('Setting.PreLayout'),
            dataIndex: 'layout',
            align: 'center',
            key: 'layout'
        },
        {
            title: this.$t('Setting.PostLayout'),
            dataIndex: 'postLayout',
            align: 'center',
            key: 'postLayout',
        },
        {
          title: this.$t('Setting.Status'),
          dataIndex: 'syncStatus',
          key: 'syncStatus',
          align: 'center',
          scopedSlots: { customRender: 'status' }
        },
        {
          title: this.$t('Setting.Progress'),
          dataIndex: 'progress',
          key: 'progress',
          align: 'center',
          scopedSlots: { customRender: 'progress' }
        }
      ]
    },
    // 已完成列
    getCompletedColumns() {
      return [
      {
          title: this.$t('Setting.StorageId'),
          dataIndex: 'storageId',
          align: 'center',
          key: 'storageId'
        },
        {
          title: this.$t('Setting.RepositoryId'),
          dataIndex: 'repositoryId',
          align: 'center',
          key: 'repositoryId'
        },
        {
          title: this.$t('Setting.TotalArtifact'),
          dataIndex: 'totalArtifact',
          key: 'totalArtifact',
          align: 'center',
        },
        {
          title: this.$t('Setting.MigratedArtifact'),
          dataIndex: 'successMount',
          key: 'successMount',
          align: 'center',
        },
        {
            title: this.$t('Setting.Layout'),
            dataIndex: 'layout',
            align: 'center',
            key: 'layout'
        },
        {
          title: this.$t('Setting.Status'),
          dataIndex: 'status',
          key: 'status',
          align: 'center',
          scopedSlots: { customRender: 'status' }
        }
      ]
    },
    getSelectedRows() {
      if (this.activeTab === 1) {
        return this.selectedPendingRows;
      } else if (this.activeTab === 2) {
        return this.selectedMigratingRows;
      }
      return [];
    },
    getSelectedKeys() {
      if (this.activeTab === 1) {
        return this.selectedPendingKeys;
      } else if (this.activeTab === 2) {
        return this.selectedMigratingKeys;
      }
      return [];
    },
    // 加载存储空间列表
    async loadStorages() {
      try {
        const response = await getStorages();
        if (response?.storages) {
          this.storageOptions = response.storages;
        }
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      }
    },
    // 根据选择的存储空间加载仓库列表
    async loadRepositories(storageId) {
      this.repositoryLoading = true;
      try {
        const params = {
          storageId: storageId,
          limit: 100000,
          type: 'hosted',
          page: 1
        };
        const response = await queryRepositoriesByStorage(params);
        if (response?.data) {
          this.repositoryOptions = response.data.rows;
        }
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      } finally {
        this.repositoryLoading = false;
      }
    },
    handleAdd() {
      this.addModalVisible = true;
    },
    handleStorageChange(value) {
      this.addForm.repositoryIds = [];
      if (value) {
        this.loadRepositories(value);
      } else {
        this.repositoryOptions = [];
      }
    },
    handleAddCancel() {
      this.addModalVisible = false;
      this.$refs.addFormRef.resetFields();
    },
    async handleAddOk() {
      try {
        await this.$refs.addFormRef.validate();
        this.addLoading = true;
        const storageId = this.addForm.storageId;
        const storeAndRepos = this.addForm.repositoryIds.map(id => (
          storageId+":"+id
        ));
        const body = {
            migrateId: this.migrateId,
            storeAndRepos: storeAndRepos
        }
        await addMigrateRepo(body);
        this.$notification.success({
          message: this.$t('Setting.Success'),
          description: this.$t('Setting.AddSuccess')
        });
        this.addModalVisible = false;
        this.$refs.addFormRef.resetFields();
        this.loadPendingData();
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      } finally {
        this.addLoading = false;
      }
    },
    async handleStartMigration() {
      console.log("selectedRows",this.getSelectedRows());
      const selectedRows = this.getSelectedRows();
      if (!selectedRows.length) {
        return;
      }

      try {
        const data = {
          migrateId: this.migrateId,
          storeAndRepos: selectedRows.map(row => (row.storageId+":"+row.repositoryId))
        }
        await startMigrate(data);
        this.$notification.success({
          message: this.$t('Setting.Success'),
          description: this.$t('Setting.MigrationStarted')
        });

        // 刷新数据
        this.loadData(this.activeTab);
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      }
    },
    filterOption(input, option) {
      return option.componentOptions.children[0].text.toLowerCase()
        .indexOf(input.toLowerCase()) >= 0;
    },
    handleSelectChange(value) {
      this.addForm.repositoryIds = value;
    },
    // 开始轮询
    startPolling() {
      if (this.pollingTimer) {
        clearInterval(this.pollingTimer);
      }
      this.pollingTimer = setInterval(() => {
        this.updateMigratingProgress();
      }, this.pollingInterval);
    },

    // 停止轮询
    stopPolling() {
      if (this.pollingTimer) {
        clearInterval(this.pollingTimer);
        this.pollingTimer = null;
      }
    },

    handlePostLayoutChange(record, value) {
      const data = {...record}
      data.postLayout = value;
      changeLayout(data).then(() => {
        this.$notification.success({
          message: this.$t('Setting.Success'),
          description: this.$t('Setting.ChangeLayoutSuccess')
        });
        this.loadPendingData();
      }).catch((error) => {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      })  
      
    },

    // 更新迁移进度
    async updateMigratingProgress() {
      try {
        // 1. 先重新获取迁移中的列表数据，不显示loading
        await this.loadMigratingData(false);
        // 2. 获取正在同步制品的记录
        const syncingRecords = this.migratingData.filter(record => record.syncStatus === 3);  
        const indexRecords = this.migratingData.filter(record => record.syncStatus === 2);
        if (!syncingRecords.length && !indexRecords.length) {
          return;
        }

        // 3. 获取同步进度
        const progressData = {
          migrateId: this.migrateId,
          storeAndRepos: syncingRecords.map(record => `${record.storageId}:${record.repositoryId}`)
        };
        const indexData = {
          migrateId: this.migrateId,
          storeAndRepos: indexRecords.map(record => `${record.storageId}:${record.repositoryId}`)
        };

        const progressResponse = await getMigrateProgress(progressData);
        const indexResponse = await getIndexProgress(indexData);
        if (progressResponse) {
          // 4. 更新数据
          this.migratingData = this.migratingData.map(record => {
            // 只更新状态为同步制品(3)的记录的进度
            if (record.syncStatus === 3||record.syncStatus === 4) {
              const key = `${record.storageId}:${record.repositoryId}`;
              const migratedArtifact = progressResponse[key];
              
              if (migratedArtifact !== undefined) {
                return {
                  ...record,
                  migratedArtifact: migratedArtifact,
                  progress: record.totalArtifact > 0 
                    ? Number(migratedArtifact)
                    : 0
                };
              }
            }
            return record;
          });
        }
        if (indexResponse) {
          this.migratingData = this.migratingData.map(record => {
            if (record.syncStatus === 2) {
              const key = `${record.storageId}:${record.repositoryId}`;
              const indexCount = indexResponse[key];
              if (indexCount !== undefined) {
                  record.totalArtifact = indexCount;
                  return record;
              }
            }
            return record;
          });
        }
      } catch (error) {
        console.error('Failed to update progress:', error);
      }
    },
    // 处理暂停迁移
    async handlePauseMigration() {
      try {
        const data = {
          migrateId: this.migrateId,
          storeAndRepos: this.selectedMigratingRows.map(row => `${row.storageId}:${row.repositoryId}`)
        };
        await pauseMigrate(data);
        this.$notification.success({
          message: this.$t('Setting.Success'),
          description: this.$t('Setting.PauseSuccess')
        });
        this.loadMigratingData();
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      }
    },
    async handleSetFailed() {
      const data = this.selectedMigratingRows.map(r=>r.id);
      await setFailed(data);
      this.$notification.success({
        message: this.$t('Setting.Success'),
      });
      this.loadMigratingData();
    },

    // 继续迁移
    async handleContinueMigration() {
      try {
        const data = {
          migrateId: this.migrateId,
          storeAndRepos: this.selectedMigratingRows.map(row => `${row.storageId}:${row.repositoryId}`)
        };
        await continueMigrate(data);
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      }
    },
    async handleComplete() {
      try {
        const data = {
          migrateId: this.migrateId,
          storeAndRepos: this.selectedCompletedRows.map(row => `${row.storageId}:${row.repositoryId}`)
        };
        await finishMigrate(data);
        this.$notification.success({
          message: this.$t('Setting.Success'),
          description: this.$t('Setting.CompleteSuccess')
        });
        this.loadCompletedData();
      } catch (error) {
        this.$notification.error({
          message: this.$t('Setting.Error'),
          description: error.message
        });
      }
    },
  },
  beforeDestroy() {
    // 组件销毁前停止轮询
    this.stopPolling();
  }
}
</script>

<style lang="scss" scoped>
.page-header {
  display: flex;
  align-items: center;
  margin-bottom: 0px;
}

.table-operations-container {
  display: flex;
  justify-content: flex-end;
  margin-bottom: 16px;
}

.table-operations {
  .ant-btn {
    margin-left: 8px;
  }
}

::v-deep .ant-select-selection--multiple {
  min-height: 32px;
  
  .ant-select-selection__choice {
    margin-top: 4px;
    margin-bottom: 4px;
  }
}

::v-deep .ant-select-dropdown-menu-item {
  padding-right: 32px;
  
  .ant-checkbox-wrapper {
    width: 100%;
  }
}
</style> 