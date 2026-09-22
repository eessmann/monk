"""Missing baseline evidence and failed samples cannot pass performance gates."""
import importlib.util
from pathlib import Path
import unittest

SPEC = importlib.util.spec_from_file_location('performance', Path(__file__).with_name('portable-performance.py'))
P = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(P)


class PerformanceTests(unittest.TestCase):
    def test_partial_cohort_cannot_be_aggregated_as_complete(self):
        available = {'fixture':'a','status':'measured','samples_ns':{'baseline':[100]*20,'candidate':[1]*20}}
        unavailable = {'fixture':'b','status':'unsupported-native-platform'}
        result = P.aggregate([available,unavailable])
        self.assertEqual(result['status'],'unverified')
        self.assertEqual(result['unavailable'],['b'])
        self.assertNotIn('at_most_10_percent_regression',result)

    def test_regression_uses_complete_twenty_sample_cohort(self):
        rows = [{'fixture':'a','status':'measured','samples_ns':{'baseline':[100]*20,'candidate':[111]*20}}]
        result = P.aggregate(rows)
        self.assertFalse(result['at_most_10_percent_regression'])
        self.assertFalse(result['lower_time'])
        self.assertEqual(result['median_ns']['candidate'],111)

    def test_missing_arithmetic_and_sample_mismatch_are_unverified(self):
        for status in ('missing-frozen-input','sample-mismatch','translation-unavailable'):
            self.assertEqual(P.aggregate([{'fixture':'a','status':status}])['status'],'unverified')
        self.assertEqual(P.aggregate([])['status'],'unverified')


if __name__ == '__main__':
    unittest.main()
