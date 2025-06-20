<template>
  <div class="wrapper">
    <div class="mx-25 search">
      <div style="display: flex; justify-content: space-between">
        <a-button type="primary" icon="plus" @click="handleAdd"> 新增政策 </a-button>
        <a-input-search placeholder="输入政策名称查询" class="v-search" v-model="queryParams.searchText" @search="handheTableSearch()" />
      </div>
    </div>
    <!-- 表格 -->
    <a-table
      rowKey="uuid"
      :columns="columns"
      :data-source="data"
      @change="handleChangeTable"
      @expand="handleExpand"
      :expandedRowKeys="expandedRowKeys"
      :scroll="{ x: true }"
      :pagination="{ pageSize: queryParams.pageSize, current: queryParams.pageNumber, total: queryParams.total, showLessItems: true }"
    >
      <!-- <a slot="action" slot-scope="text" href="javascript:;">Delete</a> -->
      <div slot="expandedRowRender" slot-scope="record" style="margin: 0">
        <!-- 名称 -->
        <div class="record-name">
          <a-form :form="infoForm" style="display: flex; justify-content: space-between">
            <a-form-item label="名称">
              <a-input v-model="record.name" @change="handleChangeName(record)" placeholder="名称" class="add-name" />
            </a-form-item>
            <a-form-item label="操作人员">
              <a-select default-value="lucy" style="width: 320px" v-model="record.operator" @change="handleChangeUser(record)">
                <a-select-option value="ANY"> 任何 </a-select-option>
                <a-select-option value="All"> 所有 </a-select-option>
              </a-select>
            </a-form-item>
            <a-form-item label="违规状态">
              <a-select default-value="lucy" style="width: 320px" v-model="record.violationState" @change="handleChangeStatus(record)">
                <a-select-option value="INFO"> 成功 </a-select-option>
                <a-select-option value="WARN"> 警告 </a-select-option>
                <a-select-option value="FAIL"> 失败 </a-select-option>
              </a-select>
            </a-form-item>
          </a-form>
        </div>
        <!-- 条件 -->
        <div class="record-conditions">
          <a-form :form="conditionsForm" style="display: flex; justify-content: space-between">
            <a-form-item label="条件" style="width: 100%">
              <div class="bar-content" v-for="i in condition" :key="i.uuid">
                <policy-condition :condition="i" :policy="policy"></policy-condition>
              </div>
              <div class="bar-add">
                <p></p>
                <a-icon type="plus-square" theme="twoTone" @click="handleAddConditions(record)" />
              </div>
            </a-form-item>
          </a-form>
        </div>
      </div>
    </a-table>
  </div>
</template>

<script>
import { getPolicyList, postPolicyList } from "@/api/policy.js";
import PolicyCondition from "./PolicyCondition.vue";
export default {
  components: {
    PolicyCondition,
  },
  created() {
    this.getData();
  },
  data() {
    return {
      columns: [{ title: "", dataIndex: "name", key: "name" }],
      data: [],
      queryParams: {
        pageNumber: 1,
        pageSize: 10,
        total: 0,
        searchText: "",
      },
      infoForm: {},
      conditionsForm: {},
      expandedRowKeys: [],
      condition: [],
      policy: {},
    };
  },
  methods: {
    getData() {
      getPolicyList(this.queryParams).then((res) => {
        this.queryParams.total = +res.headers["x-total-count"];
        this.data = res.data;
      });
    },
    handleAdd() {},
    handleExpand(expanded, record) {
      this.policy = record;
      // console.log(record);
      if (record.policyConditions && record.policyConditions.length > 0) {
        this.condition = record.policyConditions;
      }
      // 只展开一行
      if (this.expandedRowKeys.length > 0) {
        //进这个判断说明当前已经有展开的了
        //返回某个指定的字符串值在字符串中首次出现的位置，下标为0
        let index = this.expandedRowKeys.indexOf(record.uuid);
        if (index > -1) {
          //如果出现则截取这个id,1d到1相当于0，针对重复点击一个
          this.expandedRowKeys.splice(index, 1);
        } else {
          //如果没出现则截取所有id,添加点击id，0到1，针对已经有一个展开，点另一个会进入判断
          this.expandedRowKeys.splice(0, this.expandedRowKeys.length);
          this.expandedRowKeys.push(record.uuid);
        }
      } else {
        //数组长度小于0，说明都没展开，第一次点击，id添加到数组，数组有谁的id谁就展开
        this.expandedRowKeys.push(record.uuid);
      }
    },
    handheTableSearch() {
      this.queryParams.pageNumber = 1;
      this.getData();
    },
    // 修改名称
    handleChangeName(record) {
      const params = {
        uuid: record.uuid,
        name: record.name,
        operator: record.operator,
        violationState: record.violationState,
        includeChildren: false,
      };
      this.postPolicy(params);
    },
    // 修改操作人员
    handleChangeUser(record) {
      const params = {
        uuid: record.uuid,
        name: record.name,
        operator: record.operator,
        violationState: record.violationState,
        includeChildren: false,
      };
      this.postPolicy(params);
    },
    // 修改状态
    handleChangeStatus(record) {
      const params = {
        uuid: record.uuid,
        name: record.name,
        operator: record.operator,
        violationState: record.violationState,
        includeChildren: false,
      };
      this.postPolicy(params);
    },

    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.queryParams.pageNumber = pagination.current;
      }
      this.getData();
    },
    //   新增条件
    handleAddConditions(record) {
      this.condition.push({
        operator: "",
        subject: "",
        value: "",
      });
    },
    // 修改政策名
    postPolicy(params) {
      postPolicyList(params).then((res) => {
        // console.log(res);
      });
    },
  },
};
</script>

<style lang="scss" scoped>
.search {
  width: 98%;
  height: 50px;
}
.mx-25 .ant-row-flex {
  flex-wrap: wrap;
}
.v-search {
  max-width: 200px;
  width: 170px;
  min-width: 150px;
  margin-left: 5px;
  margin-bottom: 8px;
}

.bar-add {
  width: 100%;
  display: flex;
  align-items: center;
  padding: 0 12px;
  border: 1px solid #eee;

  p {
    flex: 1;
    margin: 0;
    height: 38px;
    line-height: 38px;
  }
  i {
    font-size: 20px;
    cursor: pointer;
  }
}
.bar-content {
  display: flex;
  justify-content: space-between;
}
.add-name {
  width: 320px;
}
</style>
