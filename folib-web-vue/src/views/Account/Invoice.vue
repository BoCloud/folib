<!-- 
	This is the Invoice page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div>

		<a-card :bordered="false" class="card-invoice header-solid mb-24 mx-auto my-50" style="max-width: 800px;">
			<template #title>
				<img src="images/logo-ct-black.png" class="brand">
			</template>
			<a-row :gutter="[24]" type="flex" class="invoice-header">
				<a-col :span="24" :md="8">
					<h6 class="font-semibold">
						St. Independence Embankment, 050105 Bucharest, Romania
					</h6>
					<p>
						tel: +4 (074) 1090873
					</p>
				</a-col>
				<a-col :span="24" :md="8" class="ml-auto text-right">
					<h6 class="font-semibold">
						Billed to: John Doe
					</h6>
					<p>
						4006 Locust View Drive
						San Francisco CA
						California
					</p>
				</a-col>
			</a-row>
			<a-row :gutter="[24]" type="flex" class="invoice-info mt-50">
				<a-col :span="24" :md="8">
					<h6 class="font-semibold text-muted mb-0"> Invoice no </h6>
					<h5 class="font-semibold"> #0453119 </h5>
				</a-col>
				<a-col :span="24" :md="10" class="ml-auto text-right">
					<a-row :gutter="[24]" type="flex" class="invoice-info">
						<a-col :span="24" :md="12">
							<h6 class="font-semibold text-muted mb-0"> Invoice date: </h6>
							<h6 class="font-semibold text-muted mb-0"> Due date: </h6>
						</a-col>
						<a-col :span="24" :md="12">
							<h6 class="font-semibold mb-0"> 06/03/2019 </h6>
							<h6 class="font-semibold mb-0"> 11/03/2019 </h6>
						</a-col>
					</a-row>
				</a-col>
			</a-row>
			
			<table class="table-invoice mt-30">
				<thead>
					<tr>
						<th colspan="4" class="font-semibold">Item</th>
						<th class="font-semibold">Qty</th>
						<th class="font-semibold">Rate</th>
						<th class="font-semibold">Amount</th>
					</tr>
				</thead>
				<tbody>
					<tr>
						<td colspan="4"> Premium Support </td>
						<td> 1 </td>
						<td> $ 9.00 </td>
						<td> $ 9.00 </td>
					</tr>
					<tr>
						<td colspan="4"> Soft UI Design System PRO </td>
						<td> 3 </td>
						<td> $ 100.00 </td>
						<td> $ 300.00 </td>
					</tr>
					<tr>
						<td colspan="4"> Parts for service </td>
						<td> 1 </td>
						<td> $ 89.00 </td>
						<td> $ 89.00 </td>
					</tr>
				</tbody>
				<tfoot>
					<tr>
						<th colspan="4"></th>
						<th></th>
						<th> <h5 class="font-semibold">Total</h5> </th>
						<th> <h5 class="font-semibold">$ 698</h5> </th>
					</tr>
				</tfoot>
			</table>

			<a-row :gutter="[24]" type="flex" class="invoice-message mt-50 pt-50">
				<a-col :span="24" :md="8">
					<h5 class="font-semibold">
						Thank you!
					</h5>
					<p>
						If you encounter any issues related to the invoice you can contact us at:
					</p>
				</a-col>
			</a-row>

			<a-row :gutter="[24]" type="flex" class="invoice-foot">
				<a-col :span="24" :md="12">
					<h6 class="font-semibold">
						<span class="text-muted">Email:</span> support@creative-tim.com
					</h6>
				</a-col>
				<a-col :span="24" :md="12" class="ml-auto text-right">
					<a-button type="primary" @click="print">PRINT</a-button>
				</a-col>
			</a-row>

		</a-card>

	</div>

</template>

<script>

	export default {
		data() {
			return {
			};
		},
		methods: {
			print() {
				window.print() ;
			},
		},
	};
</script>

<style>
	@media print {
		.ant-layout-sider.sider-primary,
		.sidebar-overlay {
			display: none ;
		}
	}
</style>