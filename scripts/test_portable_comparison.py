"""Evidence must expose byte/mode mismatches and unavailable execution."""
import importlib.util
import json
import os
import subprocess
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location('portable_comparison',Path(__file__).with_name('portable-comparison.py'))
E = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(E)


class EvidenceTests(unittest.TestCase):
    def test_filesystem_captures_raw_bytes_modes_and_links(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root/'data').write_bytes(b'\0\xff\n')
            (root/'data').chmod(0o640)
            (root/'link').symlink_to('data')
            actual = E.snapshot(root)
            self.assertEqual(actual['data']['bytes_base64'],'AP8K')
            self.assertEqual(actual['data']['mode'],0o640)
            self.assertEqual(actual['link']['target'],'data')

    def test_equal_streams_do_not_hide_mode_mismatch(self):
        observed = {'status':'completed','exit':0,'stdout':b'','stderr':b''}
        actual = E.effect_comparison(observed,observed,{'mode':0o640},{'mode':0o600})
        self.assertEqual(actual,{'status':'mismatch','differences':['filesystem']})

    def test_strengthened_inputs_have_observable_bash_results(self):
        expected = {
            'read-array-live-ifs': (b'status:0\nfield:<alpha>\nfield:<beta>\nfield:<>\nfield:<gamma>\n', b''),
            'read-eof-without-newline': (b'status:1\nvalue:<partial value>\n', b''),
            'read-nul-invalid-bytes': (b'status:0\nvalue:<a\xffb>\n', b''),
            'dense-array-append-observed': (b'<first item>\n<>\n<third>\n<last item>\n<*>\n', b''),
            'visible-time-output': (b'timed', b'measured\n'),
        }
        env = dict(os.environ, LC_ALL='C', LANG='C')
        for key in ('BASH_ENV', 'ENV', 'SHELLOPTS', 'BASHOPTS'):
            env.pop(key, None)
        for name, source, data in E.strengthened_cases():
            with self.subTest(case=name):
                observed = subprocess.run(['bash', '--noprofile', '--norc', '-c', source],
                                          input=data, capture_output=True, env=env, timeout=5)
                self.assertEqual((observed.returncode, observed.stdout, observed.stderr), (0, *expected[name]))

    def test_durable_manifest_preserves_historic95_denominator_and_hashes(self):
        repo = Path(__file__).resolve().parent.parent
        original = json.loads((repo / 'docs/evidence/bakeoff-2026-09-09.json').read_text())
        current = json.loads((repo / 'docs/evidence/portable-exact-cohorts-2026-09-22.json').read_text())
        self.assertEqual(current['historical_denominator'], 95)
        self.assertEqual(len(current['historic95']), 95)
        self.assertEqual({r['fixture']:r['input_sha256'] for r in current['historic95']},
                         {r['fixture']:r['input_sha256'] for r in original['fixtures']})
        self.assertEqual(len(current['strengthened_cases']), 5)
        self.assertTrue(all(r['cohort']=='strengthened' for r in current['strengthened_cases']))

    def test_candidate_launcher_preserves_script_arguments(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            inputs = root / 'inputs'
            inputs.mkdir()
            fixture = {'cohort':'historic95', 'metadata':{'fixtureMetaArgs':['', 'two words']}, 'stdin_base64':''}
            with patch.object(E.COMPARE, 'run', return_value={'status':'completed'}) as run:
                E.execute(Path('/fish'), fixture, Path('/script.fish'), inputs, root / 'cwd', {}, 5,
                          fish=True, launcher=Path('/candidate-runtime'))
            self.assertEqual(run.call_args.args[0], ['/candidate-runtime','--abi','2','launch','/script.fish','','two words'])

    def test_sourceable_caller_retains_direct_fish_observer(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            inputs = root / 'inputs'
            inputs.mkdir()
            fixture = {'cohort':'caller', 'metadata':{}, 'stdin_base64':''}
            with patch.object(E.COMPARE, 'run', return_value={'status':'completed'}) as run:
                E.execute(Path('/fish'), fixture, Path('/script.fish'), inputs, root / 'cwd', {}, 5,
                          fish=True, launcher=Path('/candidate-runtime'))
            command = run.call_args.args[0]
            self.assertEqual(command[:3], ['/fish','--no-config','-c'])
            self.assertIn('source "$argv[1]"', command[3])

    def test_stable_directory_lane_is_explicit_for_supported_translators(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / 'input.bash').write_text('pwd')
            fixture = {'fixture':'input.bash', 'cohort':'historic95', 'metadata':{}}
            for contract in ('default', 'stable'):
                for tool in ('original', 'current', 'candidate', 'babelfish'):
                    with self.subTest(contract=contract, tool=tool), patch.object(E.COMPARE, 'run', return_value={'stdout':b''}) as run:
                        E.translate(tool, Path('/translator'), Path('/runtime'), fixture, root,
                                    root / 'generated.fish', {}, 5, directory_contract=contract)
                        command = run.call_args.args[0]
                        if contract == 'stable' and tool in ('current', 'candidate'):
                            self.assertEqual(command[-2:], ['--directory-contract', 'stable'])
                        else:
                            self.assertNotIn('--directory-contract', command)

    def test_timeout_never_becomes_match(self):
        observed = {'status':'timeout','exit':-9,'stdout':b'','stderr':b''}
        self.assertEqual(E.effect_comparison(observed,observed)['status'],'unavailable')


if __name__ == '__main__':
    unittest.main()
